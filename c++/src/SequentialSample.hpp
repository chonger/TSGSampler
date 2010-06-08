#ifndef TSG_SEQSAMPLE
#define TSG_SEQSAMPLE 1

#include<stdlib.h>
#include<math.h>

struct SeqSample {
    bool* markers;
    bool* markerMask;
    ParseTree* tree;
    NodeOffset head;
    double prob;

    SeqSample() : markers(NULL), markerMask(NULL), tree(NULL) {

    }
    
    SeqSample(ParseTree* t, NodeOffset no) :
        markers(NULL), markerMask(NULL), tree(t),
        head(no), prob(1.0) {
        markers = new bool[t->size];
        markerMask = new bool[t->size];
        for(size_t i=0;i<t->size;++i) {
            markerMask[i] = false;
            markers[i] = false;
        }
    }

    SeqSample(const SeqSample& o) :
        markers(NULL), markerMask(NULL), tree(o.tree),
        head(o.head), prob(o.prob)
    {
        markers = new bool[tree->size];
        markerMask = new bool[tree->size];
        for(size_t i=0;i<tree->size;++i) {
            markers[i] = o.markers[i];
            markerMask[i] = o.markerMask[i];
        }

    }

    SeqSample& operator=(const SeqSample& o) {
        if(markers != NULL)
            delete[] markers;
        markers = NULL;
        if(markerMask != NULL)
            delete[] markerMask;
        markerMask = NULL;
        tree = o.tree;
        prob = o.prob;
        head = o.head;
        markers = new bool[tree->size];
        markerMask = new bool[tree->size];
        for(size_t i=0;i<tree->size;++i) {
            markers[i] = o.markers[i];
            markerMask[i] = o.markerMask[i];
        }
        return *this;
    }
    
    ~SeqSample() {
        if(markers != NULL)
            delete[] markers;
        markers = NULL;
        if(markerMask != NULL)
            delete[] markerMask;
        markerMask = NULL;
    }
    
    void setMarkers() {
        for(size_t i=0;i<tree->size;++i) {
            if(markerMask[i]) {
                tree->markers[i] = markers[i];
            }
        }
    }


};

struct SampleDist {
    SeqSample* samples;
    double norm;
    size_t num;
    ParseTree* tree;
    double leftOut;
    
    SampleDist() : samples(NULL), norm(0.0), num(0), tree(NULL), leftOut(0.0) {}
    
    SampleDist(size_t n, SeqSample* sam, ParseTree* t) :
        samples(NULL), norm(0.0), num(n), tree(t) {

        samples = new SeqSample[n];
        for(size_t i=0;i<n;++i) {
            samples[i] = sam[i];
            norm += samples[i].prob;

        }
        for(size_t i=0;i<n;++i) {
            samples[i].prob = samples[i].prob * .9 / norm;
            //printf("PROB - %E\n",samples[i].prob);
        }
        //printf("NORM = %E --- %d\n",norm,num);
        //the prob fields are shrunk by .9 though
        //each possible tree also gets a piece of the .1
        leftOut = 0.1 / ((double) pow(2,tree->size));
    }

    SampleDist& operator=(const SampleDist& o) {
        if(samples != NULL)
            delete[] samples;
        samples = new SeqSample[o.num];
        num = o.num;
        norm = o.norm;
        for(size_t i=0;i<num;++i) {
            samples[i] = o.samples[i];
        }
        tree = o.tree;
        leftOut = o.leftOut;
        return *this;
    }

    SeqSample sample() {
        double rDub = (double) rand() / ((double)RAND_MAX + 1.0);
        double p = rDub;
        //printf("sample! %f\n",p);
        double cumulative = 0.0;
        for(size_t i=0;i<num;++i) {
            //printf("PROB - %f\n",samples[i].prob);
            cumulative += samples[i].prob;
            if(cumulative >= p)
                return samples[i];
        }

        //the internal sample's probs should add up to .9
        if(rDub < .8999) {
            printf("randVal = %f\n",rDub);
            for(size_t i=0;i<num;++i) {
                printf("PROB - %f\n",samples[i].prob);
            }
            throw "DOESNT MATCH COMPRESSION";
        }

        SeqSample s(tree,0);
        for(size_t i=0;i<tree->size;++i) {
            if(samples[0].markerMask[i]) {
                s.markerMask[i] = true;
                double r2 = (double) rand() / ((double)RAND_MAX + 1.0);
                if(r2 > .5)
                    s.markers[i] = true;
                else
                    s.markers[i] = false;
            }
        }
        s.markers[samples[0].head] = true;
        return s;
    }

    double score(bool* marks, size_t size) {
        //first see if its in the distribution

        double ret = leftOut;
        for(size_t n=0;n<num;++n) {
            bool found = true;
            for(size_t i=0;i<size;++i) {
                if(samples[n].markers[i] != marks[i]) {
                    found = false;
                    break;
                }
            }
            if(found)
                ret += samples[n].prob;
        }
        
        //if we get here, the segmentation wasnt found
        return ret;
    }

    ~SampleDist() {
        if(samples != NULL) 
            delete[] samples;
        samples = NULL;
    }

};


#endif
