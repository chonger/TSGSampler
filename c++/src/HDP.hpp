#ifndef ENBUSKE_HDP
#define ENBUSKE_HDP 1

#include <vector>
#include "DP.hpp"
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

/**
 * TODO
 *
 * parameter resampling
 * write data
 * read data
 * 
 */ 


class HDP {
public:
    
    HDP(PCFG* pcfg_,std::ifstream& ifs,double headCut, double noTag) :
        pcfg(pcfg_) {

        nulltree1 = new ParseTree(NULL,NULL,NULL,0);
        nulltree2 = new ParseTree(NULL,NULL,NULL,0);
        baseMap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        baseMap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));

        //this reads the betas
        base = new HBase(pcfg,ifs,headCut,noTag);
        
        //read base alphas
        baseAlpha = new double[pcfg->nLHS];
        //printf("READING %d alphas\n",pcfg->nLHS);
        for(size_t i=0;i<pcfg->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(baseAlpha + i),sizeof(double));
        }

        readLEbytes(ifs,reinterpret_cast<char*>(&numDP),sizeof(size_t));

        for(size_t i=0;i<numDP;++i) {
            TreeHashMap* m = new TreeHashMap();
            m->set_empty_key(Segment(nulltree1,(NodeOffset)0));
            m->set_deleted_key(Segment(nulltree2,(NodeOffset)0));
            mixMaps.push_back(m); //empty hashmap for the dp
            TableHashMap* tabM = new TableHashMap();

            tabM->set_empty_key(Segment(nulltree1,(NodeOffset)0));
            tabM->set_deleted_key(Segment(nulltree2,(NodeOffset)0));
          
            tables.push_back(tabM); //add empty table list for the dp
    
            mixLHSCounts.push_back(new double[pcfg->nLHS]); //allocate memory for lhs counts
            //read mixture alphas
            mixAlpha.push_back(new double[pcfg->nLHS]);
            for(size_t j=0;j<pcfg->nLHS;++j) {
                readLEbytes(ifs,reinterpret_cast<char*>(mixAlpha[i] + j),sizeof(double));
            }
        }
        
        baseLHSCounts = new double[pcfg->nLHS];        
        
    }

    virtual ~HDP() {
        for(size_t i=0;i<numDP;++i) {
            delete mixMaps[i];
            delete[] mixLHSCounts[i];
            delete[] mixAlpha[i];
            delete tables[i];
        }

        delete base;
        base = NULL;
        delete[] baseAlpha;
        baseAlpha = NULL;
        delete nulltree1;
        nulltree1 = NULL;
        delete nulltree2;
        nulltree2 = NULL;
        delete baseLHSCounts;
        baseLHSCounts = NULL;
    }

    void resampleParams(double* lhsTotals) {

        //all of the trees are assigned to one of the mixDPs

        /**
         * BETA RESAMPLING
         *      Betas are the probability of LHS's being NT Leaves
         */ 

        
        //Base P Estimation of beta needs the counts of all lhs's
        double allLHSCounts[pcfg->nLHS];
        for(size_t i=0;i<pcfg->nLHS;++i) {
            allLHSCounts[i] = 0;
        }

        
        //resample base distribution betas - needs LHS counts for all data items and LHS totals
        for(size_t i=0;i<numDP;++i) {
            TreeHashMap* m = mixMaps[i];
            for(TreeHashMap::iterator iter = m->begin(); iter != m->end(); ++iter) {
                TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex]; //head of the rule
                size_t index = pcfg->lhsMap[n.index];
                allLHSCounts[index] += 1;
            }
        }

        base->resampleParams(allLHSCounts,lhsTotals);
        

        /**
         *          ALPHA RESAMPLING
         *             Alphas are the concentration parameters
         */ 
        
        //first collect the number of classes in all DPs
        double baseClasses[pcfg->nLHS];
        double mixClasses[numDP][pcfg->nLHS];
        for(size_t i=0;i<pcfg->nLHS;++i) {
            baseClasses[i] = 0;
            for(size_t j=0;j<numDP;++j) {
                mixClasses[j][i] = 0;
            }
        }

        for(TreeHashMap::iterator iter = baseMap.begin(); iter != baseMap.end(); ++iter) {
            TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
            baseClasses[pcfg->lhsMap[n.index]] += 1;
        }
        
        for(size_t i=0;i<numDP;++i) {
            TreeHashMap* m = mixMaps[i];
            for(TreeHashMap::iterator iter = m->begin(); iter != m->end(); ++iter) {
                TreeNode& n = iter->first.ptree->nodelist[iter->first.headIndex];
                mixClasses[i][pcfg->lhsMap[n.index]] += 1;
            }
        }

        //resample alphas for base 
        resampleAlpha(baseAlpha,baseClasses,baseLHSCounts);

        
        //resample alphas for each mixDP
        for(size_t i=0;i<numDP;++i) {
            resampleAlpha(mixAlpha[i],mixClasses[i],mixLHSCounts[i]);
        }

        
    }

    //calling this functions invalidates iterators!!!!
    void insertTo(char aspect, Segment& seg) {
        size_t lhsInd = pcfg->lhsMap[seg.begin().n->index];

        TreeHashMap* map = mixMaps[aspect];
                
        TreeHashMap::iterator findo = map->find(seg);
        double count = 0;
        if(findo != map->end()) {
            count = findo->second;
        }
        
        //does it sit at a new table? if so, insert to the back too
        float randval = (rand() / ((float) RAND_MAX + 1));
        std::pair<double,double> scorePair = hdpScore(seg,aspect);
        double normalized = scorePair.first / (scorePair.first + scorePair.second);

        if(randval > normalized) { //it sits at a new table
            
            baseLHSCounts[lhsInd] += 1;
            TreeHashMap::iterator bfindo = baseMap.find(seg);
            if(bfindo == baseMap.end())
                baseMap.insert(std::make_pair(seg,1));
            else
                bfindo->second += 1;
            TableHashMap::iterator tabFindo = tables[aspect]->find(seg);
            std::vector<double> newL;
            newL.push_back(1);
            if(tabFindo == tables[aspect]->end()) {
                tables[aspect]->insert(std::make_pair(seg,newL));
            } else {
                tabFindo->second.push_back(1);
            }

            //base counts should be the same as the number of tables for all rules with this LHS
            
            
        } else {
            //pick a table to sit at

            double runTotal = 0;
            randval = (rand() / ((float) RAND_MAX + 1));

            std::vector<double> tabs = tables[aspect]->find(seg)->second;
            
            for(size_t i=0;i<tabs.size();++i) {
                runTotal += tabs[i] / count;
                if(randval < runTotal) {
                    //sit here!
                    tabs[i] += 1;
                    break;
                }
            }
        }
        
        //insert to the derived map
        (mixLHSCounts[aspect])[lhsInd] += 1;
        if(findo == map->end())
            map->insert(std::make_pair(seg,1));
        else
            findo->second += 1;
    }

    //make sure not to call when the segment is not in the map!
    //does not invalidate iterators
    void removeFrom(char aspect, Segment& seg) {
        size_t lhsInd = pcfg->lhsMap[seg.begin().n->index];

        TreeHashMap* map = mixMaps[aspect];
                
        //double lhsC = (mixLHSCounts[aspect])[lhsInd];
        TreeHashMap::iterator findo = map->find(seg);
        if(findo == map->end()) {
            printf("REMOVING SOMETHING AND NOT FOUND\n");
            throw "!";
        }
        double c = (double)findo->second;

        
        double runTotal = 0;
        float randval = (rand() / ((float) RAND_MAX + 1));

        std::vector<double>& tabs = tables[aspect]->find(seg)->second;
        
        for(std::vector<double>::iterator iter = tabs.begin();
            iter != tabs.end();
            ++iter) {
            
            //printf("RUN %E %E\n",(*iter),(double)c);
            runTotal += (*iter) / c;
            if(randval < runTotal) {
          
                //remove from this table
                double tCount = (*iter);
                if(tCount == 1) {
                    //printf("HIT\n");
                    //remove this table
                    
                    tabs.erase(iter);
                    //remove from base
                    TreeHashMap::iterator bfindo = baseMap.find(seg);
                    if(baseLHSCounts[lhsInd] == 0) {
                        printf("BASE COUNT LESS THAN ZERO\n");
                        throw "!";
                    }

                    baseLHSCounts[lhsInd] -= 1;

                    if(bfindo->second == 1)
                        baseMap.erase(bfindo);
                    else
                        bfindo->second -= 1;
                    
                } else {
                    
                    (*iter) -= 1;
                    
                }

                break;
            }
        }
        //printf("DONE\n");

        //remove from derived DP
        if(findo->second == 1) {
            map->erase(findo); //does not invalidate other iterators
        } else 
            findo->second -= 1;
        (mixLHSCounts[aspect])[lhsInd] -= 1;
        if((mixLHSCounts[aspect])[lhsInd] < 0) {
            printf("MIX COUNT LESS THAN ZERO\n");
            throw "!";
        }

    }

    size_t getCount(Segment& seg, TreeHashMap* map) {
        TreeHashMap::iterator iter = map->find(seg);
        size_t count = 0;
        if(iter != map->end())
            count = iter->second;
        return count;
    }

    double score(Segment& seg, char aspect) {
        std::pair<double,double> scr = hdpScore(seg,aspect);
        double ret = scr.first + scr.second;
        if(ret == 0) {
            printf("ZERO SCORE!\n");
            throw "!";
        }
        return ret;
    }
    
    std::pair<double,double> hdpScore(Segment& seg, char aspect) {
        TreeNode& node = seg.ptree->nodelist[seg.headIndex];
        
        size_t lhs = pcfg->lhsMap[node.index];
        double total = (mixLHSCounts[aspect])[lhs];
        double al = (mixAlpha[aspect])[lhs];
        TreeHashMap* map = mixMaps[aspect];
        
        double count = getCount(seg,map); //count in the mixture member
        
        double baseScore = bScore(seg); //probability under the base

        if(isinf(baseScore) || isnan(baseScore)) {
            printf("BAD BASE SCORE %E\n",baseScore);
            throw "!";
        }
        
        //printf("%f %f %f %f\n",count,al,total,baseScore);
        
        double fromTable = (count) / (al + total);
        double fromBase = (al * baseScore) / (al + total);

        if(isinf(fromBase) || isnan(fromBase)) {
            printf("BAD B SCORE\n");
            throw "!";
        }
        if(isinf(fromTable) || isnan(fromTable)) {
            printf("BAD T SCORE\n");
            throw "!";
        }
        
        return std::make_pair(fromTable,fromBase);
    }
    
    double bScore(Segment& seg) {

        TreeNode& node = seg.ptree->nodelist[seg.headIndex];
        
        size_t lhs = pcfg->lhsMap[node.index];
        double total = baseLHSCounts[lhs];
        double al = baseAlpha[lhs];

        TreeHashMap::iterator iter = baseMap.find(seg);
        double count = 0;
        if(iter != baseMap.end())
            count = iter->second;

        std::pair<double,double> baseScore = base->score(seg);

        //TODO : This might be bad!!!! rounds all things lower than 10^-300 to 10^-300
        if((baseScore.first / baseScore.second) < pow(10,-300)) {
            return pow(10,-300);
        }

        
        double unsmoothed = (count * baseScore.second + al * baseScore.first) / (al + total);

        double ret = unsmoothed / baseScore.second;
        if(isinf(ret)) {
            printf("!!!%f %f %f %f %f\n",count,al,total,baseScore.first,baseScore.second);
            throw "!";
        }
        return unsmoothed / baseScore.second;
    }

    void write(std::ofstream& ofs) { 

        base->write(ofs);
        
        //write alphas
        for(size_t i=0;i<pcfg->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(baseAlpha + i),sizeof(double));
        }

        writeBEbytes(ofs,reinterpret_cast<char*>(&numDP),sizeof(size_t));

        for(size_t i=0;i<numDP;++i) {
            for(size_t j=0;j<pcfg->nLHS;++j) {
                writeBEbytes(ofs,reinterpret_cast<char*>(mixAlpha[i] + j),sizeof(double));
            }   
        }
        

    }


    void printStats() {
        printf("BASE DP - %d\n",baseMap.size());

        //for(size_t j=0;j<pcfg->nLHS;++j) {
        //    printf("NTC %d - %E\n",j,baseLHSCounts[j]);
        //}
        for(size_t i=0;i<mixMaps.size();++i) {
            printf("MIX  DP - %d\n",mixMaps[i]->size());
            //for(size_t j=0;j<pcfg->nLHS;++j) {
            //    printf("NTC %d %d - %E\n",i,j,mixLHSCounts[i][j]);
            //}
        }
    }

    void clearAll() {
        baseMap.clear();
        for(size_t j=0;j<numDP;++j) {
            mixMaps[j]->clear();
            tables[j]->clear();
        }
        for(size_t i=0;i<pcfg->nLHS;++i) {
            baseLHSCounts[i] = 0;
            for(size_t j=0;j<numDP;++j) {
                mixLHSCounts[j][i] = 0;
            }
        }

    }
    
    ParseTree* nulltree1;
    ParseTree* nulltree2;

    
    
    PCFG* pcfg;
    BASE* base;
    TreeHashMap baseMap;
    double* baseLHSCounts;
    double* baseAlpha;
    size_t numDP;
    std::vector<TreeHashMap*> mixMaps;
    std::vector<double*> mixLHSCounts;
    std::vector<double*> mixAlpha;
    //for each dp and for each LHS, keep a list of table counts
    std::vector<TableHashMap*> tables;

private:

    void resampleAlpha(double* alpha,double* numClasses,double* lhsTotals) {

        double ALPHA_SIGSQ = 100;
        double GAMMA_A = .001;
        double GAMMA_B = 1000;
        double LOW_BOUND = .1;
        
        gsl_rng* r = gsl_rng_alloc(gsl_rng_taus);
        
        //printf("RESAMPLED ALPHAS\n");
        for(size_t i=0;i<pcfg->nLHS;++i) {
            double curAlpha = alpha[i];
            double numC = numClasses[i];
            double total = lhsTotals[i];

            //printf("RESAMP DATA A-%E K-%d N-%d\n",curAlpha,(size_t)numC,(size_t)total);
            
            //this was == 0, but I got some floating point errors, these should be counts anyways.
            if(total < 1) //dont resample the alpha if this DP is empty
                continue;
            
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
            if(nextAlpha < LOW_BOUND)
                continue;
            if(accept >= 1.0) 
                alpha[i] = nextAlpha;
            else {
                double rando = ((double)rand() / (double) RAND_MAX);
                if(rando <= accept) {
                    alpha[i] = nextAlpha;
                }
            }

            //printf("ALPHA %f\n",nextAlpha);
            
        }
        
        gsl_rng_free(r);

    }
    
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
    
};


#endif
