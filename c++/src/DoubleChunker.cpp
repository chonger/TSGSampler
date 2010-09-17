#include "DoubleChunker.hpp"

using namespace std;

char DoubleChunker::sampleAspect(std::vector<double>& scores, double total) {
    float randval = (rand() / ((float) RAND_MAX + 1));
    double runTotal = 0.0;
    //printf("SAMPLE ASPECT - %f\n",total);
    for(size_t i=0;i<scores.size();++i) {
        //printf("PART - %f\n",scores[i]);
        runTotal += scores[i] / total;
        if(randval < runTotal)
            return i;
    }
    for(size_t i=0;i<scores.size();++i) {
        printf("PART - %E\n",scores[i]);
    }
    printf("TOTL - %E\n",runTotal);
    
    throw "!";
}

void DoubleChunker::sampleNode(SampleData& sample) {

    //printf("SAMPLE\n");
    
    NodeOffset offset = sample.offset;
    if(offset == 0) //TODO - HANDLE SAMPLING FROM ROOT
        return;
    ParseTree* tree = sample.tree;
    size_t dataIndex = sample.dataIndex;
    
    TreeNode& node = tree->nodelist[offset];
    NodeOffset headOffset = offset - node.head;
    TreeNode& head = tree->nodelist[headOffset];

    size_t headLHS = pcfg->lhsMap[head.index];
    size_t nodeLHS = pcfg->lhsMap[node.index];
    
    bool wasCut = tree->markers[offset];
    
    tree->markers[offset] = true; //pretend to split at the offset

    ///        GET TOP INFO
    Segment top = Segment(tree,headOffset);
    Segment bottom = Segment(tree,offset);

    /**
    size_t treeKronDel = top.equals(bottom) ? 1 : 0;
    size_t lhsKronDel = dblData->lhsMap[head.index] == dblData->lhsMap[node.index] ? 1 : 0;
    */
    
    ///         GET JOIN INFO
    tree->markers[offset] = false; //this will make the iterator find the join segment
    Segment join = Segment(tree,headOffset);

    if(wasCut) {
        //remove top and bottom
        hdp->removeFrom(head.aspect,top);
        hdp->removeFrom(node.aspect,bottom);
    } else {
        //remove join
        hdp->removeFrom(head.aspect,join);
    }

    std::vector<double> scoresJ;
    double jTotal = 0.0;
    std::vector<double> scoresT;
    double tTotal = 0.0;
    std::vector<double> scoresB;
    double bTotal = 0.0;

    //TODO !!! these dont consider the kroneker deltas!
    
    for(size_t i=0;i<hdp->numDP;++i) {
        double mixH = mixWeights[dataIndex][i][headLHS];
        double mixN = mixWeights[dataIndex][i][nodeLHS];
        if(mixH > 0) {

            double j = hdp->score(join,i) * mixH;
            //printf("scoring j in mixture dp %d - %E\n",i,j);
            jTotal += j;
            scoresJ.push_back(j);

            double t = hdp->score(top,i) * mixH;
            //printf("scoring  t in mixture dp %d - %E\n",i,t);
            tTotal += t;
            scoresT.push_back(t);

            if(isinf(t)) {
                printf("BAD T - %E\n",t);
                throw "!";

            }
            if(isinf(j)) {
                printf("BAD J - %E\n",j);
                throw "!";
            }
            

        } else {
            scoresJ.push_back(0);
            scoresT.push_back(0);
        }
        
        if(mixN > 0) {
            double b = hdp->score(bottom,i) * mixN;
            //printf("scoring  b in mixture dp %d - %E\n",i,b);
            bTotal += b;
            scoresB.push_back(b);
            if(isinf(b)) {
                printf("BAD B - %E\n",b);
                throw "!";
            }
        } else {
            scoresB.push_back(0);
        }
    }

    //printf("SAMPLE TOTALS %E %E %E\n",tTotal,bTotal,jTotal);
    
    double combProb = tTotal * bTotal;    
    double cutoff = combProb / (jTotal + combProb);
    float randval = (rand() / ((float) RAND_MAX + 1));

    //Decide which one to insert into, based on a their probabilities in each DP
    //be carefull about things like the kronDeltas for identical assignments    
    
    if(randval > cutoff) { //join, dont cut

        //node is already marked as not cut from above in this method

        //sample the aspect for join
        char asp = sampleAspect(scoresJ,jTotal);
        //printf("JOIN! %d\n",asp);
        hdp->insertTo(asp,join);

        /**
         * to update the head offset in a join,
         * everything in the bottom segment will now pointing to headOffset
         * this includes stub nodes at the leaves of the segment
         * so add in the difference
         */   
        for(Segment::iterator iter = bottom.begin();iter != bottom.end(); ++iter) {
            iter.n->head = iter.offset - headOffset;
        }
        
        head.aspect = asp;
        
    } else { //
        //the marker was set to false in this method, so set it true
        tree->markers[offset] = true;

        char aspT = sampleAspect(scoresT,tTotal);
        char aspB = sampleAspect(scoresB,bTotal);

        //printf("CUT! %d %d\n",aspT,aspB);
        
        hdp->insertTo(aspT,top);
        hdp->insertTo(aspB,bottom);

        /**
         * To update the head offsets after a cut, everything but the first node
         * in the bottom segment should be set to bottom's offset
         *
         */ 
        NodeOffset newHead = offset;
        for(Segment::iterator iter = ++bottom.begin();iter != bottom.end(); ++iter) {
            iter.n->head = (iter.offset - newHead);
        }

        node.aspect = aspB;
        head.aspect = aspT;
    }       
}


void DoubleChunker::packResults(const char* filename) {
    
    printf("Writing data to %s\n",filename);
    std::ofstream ofs(filename,std::ios::binary);
    
    pcfg->write(ofs);

    writeBEbytes(ofs,reinterpret_cast<char*>(&numTreeSets),sizeof(size_t));
    for(size_t i=0;i<numTreeSets;++i) {
        treeData[i]->write(ofs);
    }
        
    hdp->write(ofs);

    for(size_t i=0;i<numTreeSets;++i) {
        for(size_t j=0;j<hdp->numDP;++j) {
            for(size_t k=0;k<pcfg->nLHS;++k) {
                double v = mixWeights[i][j][k];
                writeBEbytes(ofs,reinterpret_cast<char*>(&v),sizeof(double));
            }
        }
    }
    
    ofs.close();
}

void DoubleChunker::resample(size_t iterations) {
    
    for(size_t i=0;i<iterations;++i) {
        if(stop)
            return;

        clock_t start = clock();

        //shuffle
        for(size_t j=1;j<samples.size() - 1;++j) {
            size_t ind = rand() % (samples.size() - j);
            SampleData tmp = samples[ind];
            samples[ind] = samples[samples.size() - j];
            samples[samples.size() - j] = tmp;
        }
        
        printf("Sampling, %d samples\n",samples.size());

        hdp->printStats();

        
        for(std::vector<SampleData >::iterator iter = samples.begin();
            iter != samples.end(); ++iter) {
            //if(stop)
            //    return;
            sampleNode(*iter);

        }

        hdp->resampleParams();
        
        double logl = 0.0;
        
        hdp->clearAll();
        for(std::vector<TreeData*>::iterator iter = treeData.begin();
            iter != treeData.end(); ++iter) {

            TreeData* td = *iter;
            
            for(size_t i=0;i<td->ntrees;++i) {
                ParseTree& pt = td->trees[i];
                
                for(size_t j=0;j<pt.size;++j) {
                    if(pt.markers[j]) {
                        Segment seg(&pt,j);
                        logl += log(hdp->score(seg,pt.nodelist[j].aspect));
                        hdp->insertTo(pt.nodelist[j].aspect,seg);
                    }
                }
            }
        }
        
        clock_t finish = clock();
        printf("Iteration %d took %f seconds\n",i,(double(finish) - double(start))/CLOCKS_PER_SEC);
        printf("Log Likelihood           : %f\n",logl);
        //printf("Number of changed labels : %d out of %d\n",numChanged,totalLabels);
        
        outstream << logl << "\n";
    }
}
