#ifndef TSG_DP
#define TSG_DP 1

#include <fstream>
#include <iostream>
#include "Util.hpp"
#include <google/dense_hash_map>
#include <gsl/gsl_math.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_sf_gamma.h>
#include "ParseTree.hpp"
#include "TreeData.hpp"
#include "math.h"

typedef google::dense_hash_map<Segment,size_t,SegmentHash,SegmentEq> TreeHashMap;

class BASE {
public:

    virtual ~BASE() {}

    virtual std::pair<double,double> score(Segment& seg) = 0;
    virtual void write(std::ofstream& ofs) = 0;
    virtual void resampleParams() = 0;
    
};

class CGBBase : public BASE {
public:

    CGBBase(TreeData* tData_, std::ifstream& ifs,double* lhsCounts_) :
        tData(tData_), lhsCounts(lhsCounts_) {

        //read betas
        beta = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(beta + i),sizeof(double));
            //printf("BETA = %f\n",betas[i]);
        }
    }

    ~CGBBase() {
        delete[] beta;
        beta = NULL;
    }
    
    std::pair<double,double> score(Segment& seg) {
        double score = 1.0;
        double scale = 1.0;
        bool first = true;
        
        double maxScale = pow(10,300);
        
        for(Segment::iterator iter = seg.begin();iter != seg.end();++iter) {
            
            if(scale < maxScale) {
                score *= 100;
                scale *= 100;
            }
            
            int index = iter.n->index;
            size_t lhs = tData->lhsMap[index];
            
            if(iter.stub) {
                score *= beta[lhs];
            } else {
                score *= tData->pcfg[index]; //PCFG score
                
                if(!first)
                    score *= (1.0 - beta[lhs]);
                else
                    first = false;
            }
        }
        
        if(score == 0) {
            printf("UNDERFLOW\n");
            //throw "!";
        }
        
        return std::make_pair(score,scale);
    }

    void resampleParams() {
        double prior = 100;
    
        gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
        for(size_t i=0;i<tData->nLHS;++i) {
            size_t expCount = lhsCounts[i];
            size_t nExpCount = tData->lhsTotals[i] - expCount;
            double newVal = gsl_ran_beta(r,prior+nExpCount,prior+expCount);
            
            //avoid zeroing segment probabilities
            if(newVal == 1.0)
                newVal = .9999;
            if(newVal == 0.0)
                newVal = .0001;
            
            beta[i] = newVal;
        }
        gsl_rng_free(r);   
    }

    void write(std::ofstream& ofs) {
        //write betas
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(beta + i),sizeof(double));
        }
    }

    TreeData* tData;
    double* lhsCounts;
    double* beta;
};

class HBase : public CGBBase {
public:
    //arguments are penalties
    HBase(TreeData* tData_, std::ifstream& ifs,double* lhsCounts_,double headCut_,double noTag_) :
          CGBBase(tData_,ifs,lhsCounts_), headCut(headCut_), noTag(noTag_) {
        
    }

    std::pair<double,double> score(Segment& seg) {
        double score = 1.0;
        double scale = 1.0;
        bool first = true;
        
        bool hasTag = false;
        
        double maxScale = pow(10,300);
        
        for(Segment::iterator iter = seg.begin();iter != seg.end();++iter) {
            
            if(scale < maxScale) {
                score *= 100;
                scale *= 100;
            }
            
            int index = iter.n->index;
            size_t lhs = tData->lhsMap[index];
            
            if(iter.stub) {
                //triggers if a node is not a terminal but is a leaf
                //so it must have children, maybe a parent. ROOT -> S is the exception
                //the "good" case is one where the leaf and its parent have different heads
                //if leaf and parent have the same head, we must be cutting a head chain
                //b/c one of the leaf's children has the same head (by def)
                
                
                if(iter.offset != 0) {
                    TreeNode* leaf = iter.n;
                    //par will be the same node at the root
                    TreeNode& par = seg.ptree->nodelist[iter.offset - leaf->parent];
                    if(leaf->lexHead == par.lexHead) {
                        scale *= pow(10,headCut);
                    }
                } 
                

                //printf("STUB\n");
                
                score *= beta[lhs];
            } else {
                
                if(iter.n->type == 1)
                    hasTag = true;                
                
                score *= tData->pcfg[index]; //PCFG score
                
                if(!first)
                    score *= (1.0 - beta[lhs]);
                else
                    first = false;
            }
        }
        
        
        if(score == 0) {
            printf("UNDERFLOW\n");
            //throw "!";
        }
        
        if(hasTag)
            return std::make_pair(score,scale);
        else
            return std::make_pair(score,scale * pow(10,headCut));
        
    }

private:
    double headCut,noTag;

};

class DP {
public:

    DP() {}
    
    DP(TreeData* tData_, std::ifstream& ifs) :
        tData(tData_)
    {

        nulltree1 = new ParseTree(NULL,NULL,0);
        nulltree2 = new ParseTree(NULL,NULL,0);
        treemap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        treemap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));

        lhsCounts = new double[tData->nLHS];
        
        fromTreeData();
        
        //read alphas
        alpha = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(alpha + i),sizeof(double));
            //printf("ALPHA = %f\n",alphas[i]);
        }
    }


    
    virtual ~DP() {
        delete base;
        base = NULL;
        delete[] alpha;
        alpha= NULL;
        delete nulltree1;
        nulltree1 = NULL;
        delete nulltree2;
        nulltree2 = NULL;
        delete lhsCounts;
        lhsCounts = NULL;
    }

    void fromTreeData() {

        for(size_t i=0;i<tData->nLHS;++i) {
            lhsCounts[i] = 0;
        }
        treemap.clear();
        
        for(size_t i=0;i<tData->ntrees;++i) {
            ParseTree& pt = tData->trees[i];

            for(size_t j=0;j<pt.size;++j) {

                size_t lhsInd = tData->lhsMap[pt.nodelist[j].index];
                
                if(pt.markers[j]) {
                    lhsCounts[lhsInd] += 1;
                    Segment seg(&pt,j);
                    TreeHashMap::iterator findo = treemap.find(seg);
                    if(findo == treemap.end())
                        treemap.insert(std::make_pair(seg,1));
                    else
                        findo->second += 1;
                }
            }
        }

    }
    
    double dpScore(Segment& seg) {
        TreeNode& node = seg.ptree->nodelist[seg.headIndex];
        
        size_t lhs = tData->lhsMap[node.index];
        size_t total = lhsCounts[lhs];
        double al = alpha[lhs];
        

        size_t count = getCount(seg);
        
        std::pair<double,double> baseScore = base->score(seg);
        
        //printf("%E\n",baseScore);
        
        if((baseScore.first / baseScore.second) < pow(10,-300)) {
            return pow(10,-300);
        }
        
        double unsmoothed = (count * baseScore.second + al * baseScore.first) / (al + total);

        //printf("count %d, total %d, al %E, base %E / %E \n",count,total,al,baseScore.first,baseScore.second);
        //printf("%E\n",baseScore.first / baseScore.second);
            

        return unsmoothed / baseScore.second;
    }
    
    std::pair<double,double> bScore(Segment& seg) {
        return base->score(seg);
    }

    double logLikelihood(ParseTree* trees, size_t num) {
        double ll = 0.0;
        //verb = true;
        /**        
        for(size_t i=0;i<num;++i) {
            ParseTree& tree = trees[i];
            for(NodeOffset j=0;j<tree.size;++j) {
                if(tree.markers[j]) {
                    Segment seg = Segment(&tree,j);
                    double segScore = dpScore(seg);
                    if(segScore == 0) {
                        for(NodeOffset k=0;k<tree.size;++k) {
                            printf("%d",tree.markers[k]);
                        }
                        printf("\n");
                        for(NodeOffset k=0;k<tree.size;++k) {
                            printf("%d",tree.nodelist[k].type);
                        }
                        printf("\n");
                        printf("at index %d\n",j);
                        printf("!!!!ZERO SCORE ON TREE %d\n",i);
                        throw "!";
                    }
                }
            } 
        }
        printf("ITS CLEAR\n");
        */
        treemap.clear();
        for(size_t i=0;i<num;++i) {
            ParseTree& tree = trees[i];
            for(NodeOffset j=0;j<tree.size;++j) {
                if(tree.markers[j]) {
                    Segment seg = Segment(&tree,j);
                    double segScore = dpScore(seg);
                    if(segScore == 0) {
                        for(NodeOffset k=0;k<tree.size;++k) {
                            printf("%d",tree.markers[k]);
                        }
                        printf("\n");
                        for(NodeOffset k=0;k<tree.size;++k) {
                            printf("%d",tree.nodelist[k].type);
                        }
                        printf("\n");
                        printf("at index %d\n",j);
                        printf("ZERO SCORE ON TREE %d\n",i);
                        dpScore(seg);
                        throw "!";
                    }
                    ll += log(segScore);
                    
                    TreeHashMap::iterator it = treemap.find(seg);
                    if(it == treemap.end())
                        treemap.insert(std::make_pair(seg,1));
                    else {
                        it->second = it->second + 1;
                    }                
                }
            } 
        }
        return ll;
        
    }

    void resampleParams() {
    
        double ALPHA_SIGSQ = 30;
        double GAMMA_A = .001;
        double GAMMA_B = 1000;
        
        gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
        size_t numClasses[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) { numClasses[i] = 0; }
        
        for(TreeHashMap::iterator iter = treemap.begin(); iter != treemap.end(); ++iter) {
            TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
            numClasses[tData->lhsMap[n.index]] += 1;
        }
        
        for(size_t i=0;i<tData->nLHS;++i) {
            double curAlpha = alpha[i];
            double numC = numClasses[i];
            double total = tData->lhsTotals[i];
            
            
            std::pair<double,double> curMV = getLNMeanVar(curAlpha,ALPHA_SIGSQ);
            double nextAlpha = gsl_ran_lognormal(r,curMV.first,curMV.second);
            std::pair<double,double> nextMV = getLNMeanVar(nextAlpha,ALPHA_SIGSQ);
            double qFrac = gsl_ran_lognormal_pdf(curAlpha,nextMV.first,nextMV.second);
            
            qFrac /= gsl_ran_lognormal_pdf(nextAlpha,curMV.first,curMV.second);
            
            
            double pFrac = evalGammaPosterior(nextAlpha,GAMMA_A,GAMMA_B,numC,total);
            
            pFrac /= evalGammaPosterior(curAlpha,GAMMA_A,GAMMA_B,numC,total);
            
            double accept = qFrac * pFrac;
            
            if(nextAlpha == 0)
                continue;
            if(accept >= 1.0) 
                alpha[i] = nextAlpha;
            else {
                double rando = ((double)rand() / (double) RAND_MAX);
                if(rando <= accept) {
                    alpha[i] = nextAlpha;
                }
            }
        }
        
        gsl_rng_free(r);

        base->resampleParams();
        
    }

    size_t getCount(Segment& seg) {
        TreeHashMap::iterator iter = treemap.find(seg);
        size_t count = 0;
        if(iter != treemap.end())
            count = iter->second;
        return count;
    }

    TreeHashMap treemap;
    
    std::pair<double,double> getLNMeanVar(double d, double variance) {
        double v = log (variance / pow(d,2.0) + 1);
        double m = log(d) - v/2;
        return std::make_pair(m,v);
    }
    
    double evalGammaPosterior(double d, double gamma_a, double gamma_b, double k, double n) {
        double ret = log(gsl_ran_gamma_pdf(d,gamma_a,gamma_b));
        ret += log(d) * (k-1);
        ret += log(d + n);
        ret += gsl_sf_lnbeta(d+1,n);
        ret = exp(ret);
        return ret;
    }
    
    /**
    std::pair<double,double> getLNMeanVar(double d, double variance);
    double evalGammaPosterior(double d, double gamma_a, double gamma_b, double k, double n);
    */
    
    void write(std::ofstream& ofs) {
        //write alphas
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(alpha + i),sizeof(double));
        }

        base->write(ofs);
    }

    TreeData* tData;
    double* alpha;
    BASE* base;
    
    ParseTree* nulltree1;
    ParseTree* nulltree2;

    double* lhsCounts;
};

class NormalDP : public DP {
public:
    NormalDP(TreeData* tData_, std::ifstream& ifs)
        : DP(tData_,ifs) {
        base = new CGBBase(tData,ifs,lhsCounts);
    }
};

class TagDP : public DP {
public:
    TagDP(TreeData* tData_, std::ifstream& ifs,double headCut, double noTag) :
        DP(tData_,ifs) {
        base = new HBase(tData,ifs,lhsCounts,headCut,noTag);
    }
};

class CopyDP : public DP {
public:
    CopyDP(DP* o) {
        tData = o->tData;
        alpha = o->alpha;
        nulltree1 = new ParseTree(NULL,NULL,0);
        nulltree2 = new ParseTree(NULL,NULL,0);
        treemap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        treemap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));

        lhsCounts = new double[tData->nLHS];
        
        base = o->base;
        
        fromTreeData();        
    }

    ~CopyDP() {
        delete nulltree1;
        nulltree1 = NULL;
        delete nulltree2;
        nulltree2 = NULL;
        delete lhsCounts;
        lhsCounts = NULL;
    }

};


#endif