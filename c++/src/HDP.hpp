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

    void resampleParams() {

        //TODO : param resampling
        
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
    
};


#endif
