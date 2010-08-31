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
    HDP(DoubleData* tData_, std::ifstream& ifs, unsigned char numMixture_, double headCut, double noTag) :
        tData(tData_) {

        nulltree1 = new ParseTree(NULL,NULL,0);
        nulltree2 = new ParseTree(NULL,NULL,0);
        baseMap.set_empty_key(Segment(nulltree1,(NodeOffset)0));
        baseMap.set_deleted_key(Segment(nulltree2,(NodeOffset)0));

        //read base alphas
        baseAlpha = new double[tData->nLHS];
        for(size_t i=0;i<tData->nLHS;++i) {
            readLEbytes(ifs,reinterpret_cast<char*>(baseAlpha + i),sizeof(double));
        }

        baseLHSCounts = new double[tData->nLHS];
        
        numMixture = numMixture_;

        for(size_t i=0;i<numMixture;++i) {
            TreeHashMap* m = new TreeHashMap();
            m->set_empty_key(Segment(nulltree1,(NodeOffset)0));
            m->set_deleted_key(Segment(nulltree2,(NodeOffset)0));
            mixMaps.push_back(m);
            std::vector<double> tab;
            tables.push_back(tab);
            mixLHSCounts.push_back(new double[tData->nLHS]);
            //read mixture alphas
            mixAlpha.push_back(new double[tData->nLHS]);
            for(size_t j=0;j<tData->nLHS;++j) {
                readLEbytes(ifs,reinterpret_cast<char*>(mixAlpha[i] + j),sizeof(double));
            }
        }
        
        for(size_t i=0;i<tData->nLHS;++i) {
            std::vector<double> mixW;
            for(size_t j=0;j<numMixture;++j) {
                double w = 0;
                readLEbytes(ifs,reinterpret_cast<char*>(&w),sizeof(double));
                mixW.push_back(w);
            }
            mixWeights.push_back(mixW);
        }
        
        base = new HBase(tData,ifs,baseLHSCounts,headCut,noTag);

        //SET THE ASPECTS OF THE TREES.
        //TODO : This should be the job of the DoubleChunker

        //Could be better, start with all background trees in the BG, and all spec in Spec
        for(size_t i=0;i<tData->ntrees;++i) {
            ParseTree& pt = tData->trees[i];

            for(size_t j=0;j<pt.size;++j) {
                if(pt.markers[j]) { 
                    if(i <= tData->backGend)
                        pt.nodelist[j].aspect = 0;
                    else
                        pt.nodelist[j].aspect = 1;
                }
            }
        }
        
        fromTData();
    }

    virtual ~HDP() {
        for(size_t i=0;i<numMixture;++i) {
            delete mixMaps[i];
            delete[] mixLHSCounts[i];
            delete[] mixAlpha[i];
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

    void resampleParams() {

        //TODO : param resampling
        
    }
    
    void fromTData() {

        for(size_t i=0;i<tData->nLHS;++i) {
            baseLHSCounts[i] = 0;
        }
        baseMap.clear();

        for(size_t i=0;i<numMixture;++i) {
            for(size_t j=0;j<tData->nLHS;++j) {
                (mixLHSCounts[i])[j] = 0;
            }
            mixMaps[i]->clear();
        }
        
        for(size_t i=0;i<tData->ntrees;++i) {
            ParseTree& pt = tData->trees[i];

            for(size_t j=0;j<pt.size;++j) {
                if(pt.markers[j]) { //a segment is starting from here
                    Segment seg(&pt,j);
                    insertTo(pt.nodelist[j].aspect,seg);
                }
            }
        }

    }

    //calling this functions invalidates iterators!!!!
    void insertTo(char aspect, Segment& seg) {
        size_t lhsInd = tData->lhsMap[seg.begin().n->index];

        TreeHashMap* map = mixMaps[aspect];
                
        double lhsC = (mixLHSCounts[aspect])[lhsInd];
        TreeHashMap::iterator findo = map->find(seg);
        
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
            tables[aspect].push_back(1);
        } else {
            //pick a table to sit at

            double runTotal = 0;
            randval = (rand() / ((float) RAND_MAX + 1));

            for(size_t i=0;i<tables[aspect].size();++i) {
                runTotal += tables[aspect][i] / lhsC;
                if(randval < runTotal) {
                    //sit here!
                    tables[aspect][i] += 1;
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
        size_t lhsInd = tData->lhsMap[seg.begin().n->index];

        TreeHashMap* map = mixMaps[aspect];
                
        double lhsC = (mixLHSCounts[aspect])[lhsInd];
        TreeHashMap::iterator findo = map->find(seg);

        double runTotal = 0;
        float randval = (rand() / ((float) RAND_MAX + 1));

        for(std::vector<double>::iterator iter = tables[aspect].begin();
            iter != tables[aspect].end();
            ++iter) {
            
            runTotal += (*iter) / lhsC;
            if(randval < runTotal) {
                //remove from this table
                double tCount = (*iter);
                if(tCount == 1) {
                    //remove this table
                    tables[aspect].erase(iter);
                    //remove from base
                    TreeHashMap::iterator bfindo = baseMap.find(seg);
                    baseLHSCounts[lhsInd] -= 1;
                    if(bfindo->second == 1)
                        baseMap.erase(bfindo);
                    else
                        bfindo->second -= 1;
                    break;
                } else {
                    (*iter) -= 1;
                    break;
                }
            }
        }

        //remove from derived DP
        if(findo->second == 1) {
            map->erase(findo); //does not invalidate other iterators
        } else 
            findo->second -= 1;
        (mixLHSCounts[aspect])[lhsInd] -= 1;

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
        
        size_t lhs = tData->lhsMap[node.index];
        double total = (mixLHSCounts[aspect])[lhs];
        double al = (mixAlpha[aspect])[lhs];
        TreeHashMap* map = mixMaps[aspect];
        
        double count = getCount(seg,map); //count in the mixture member
        
        double baseScore = bScore(seg); //probability under the base

        //printf("%f %f %f %f\n",count,al,total,baseScore);
        
        double fromTable = (count) / (al + total);
        double fromBase = (al * baseScore) / (al + total);

        return std::make_pair(fromTable,fromBase);
    }
    
    double bScore(Segment& seg) {

        TreeNode& node = seg.ptree->nodelist[seg.headIndex];
        
        size_t lhs = tData->lhsMap[node.index];
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
        //printf("!!!%f %f %f %f %f\n",count,al,total,baseScore.first,baseScore.second);
        double unsmoothed = (count * baseScore.second + al * baseScore.first) / (al + total);

        return unsmoothed / baseScore.second;
    }

    void write(std::ofstream& ofs) { //TODO!!!!!

        printf("START WRITING!\n");
        
        //write alphas
        for(size_t i=0;i<tData->nLHS;++i) {
            writeBEbytes(ofs,reinterpret_cast<char*>(baseAlpha + i),sizeof(double));
        }

        for(size_t i=0;i<numMixture;++i) {
            for(size_t j=0;j<tData->nLHS;++j) {
                writeBEbytes(ofs,reinterpret_cast<char*>(mixAlpha[i] + j),sizeof(double));
            }   
        }
        
        for(size_t i=0;i<tData->nLHS;++i) {
            for(size_t j=0;j<numMixture;++j) {
                writeBEbytes(ofs,reinterpret_cast<char*>(&(mixWeights[i][j])),sizeof(double));
            }
        }
        
        base->write(ofs);

        printf("DONE WRITING!\n");
    }

    
    unsigned char numMixture;
    std::vector<std::vector<double> > mixWeights;
    
private:

    ParseTree* nulltree1;
    ParseTree* nulltree2;

    BASE* base;
    DoubleData* tData;
    TreeHashMap baseMap;
    double* baseLHSCounts;
    double* baseAlpha;
    std::vector<TreeHashMap*> mixMaps;
    std::vector<double*> mixLHSCounts;
    std::vector<double*> mixAlpha;
    std::vector<std::vector<double> > tables;

};


#endif
