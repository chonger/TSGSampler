#ifndef TSG_PARSETREE
#define TSG_PARSETREE 1

#include <utility>
#include <stdio.h> //for printf

/**
 *  This file defines TreeNode, ParseTree, and Segment, the main
 *  data structures used in the sampler.  A Segment corresponds
 *  to an elementary tree in the literature on TSG's.  A ParseTree
 *  is basically an ordered list of TreeNodes, representing a parse
 *  tree in DFS order.
 */ 


typedef int NodeIndex; //max rule index is 31006, but give it lots of room
typedef unsigned char NodeOffset; //maximum treebank tree size is 395, making max nts < 256
//NOTE : The scala packager currently discards trees with more than 255 nonterminals
//       This is only because of this choice of data type

struct TreeNode {
    TreeNode() {}
    TreeNode(NodeIndex index_,
             bool isTerminal_,
             NodeOffset head_,
             NodeOffset parent_,
             NodeOffset sibling_,
             NodeOffset lexHead_,
             char type_,
             char aspect_,
             NodeOffset foot_,
             NodeOffset segTop_) :
        index(index_), isTerminal(isTerminal_),
        head(head_), parent(parent_), sibling(sibling_),
        lexHead(lexHead_), type(type_), aspect(aspect_),
        foot(foot_), segTop(segTop_) {};

    TreeNode& operator=(const TreeNode& o) {
        index = o.index;
        isTerminal = o.isTerminal;
        head = o.head;
        parent = o.parent;
        sibling = o.sibling;
        lexHead = o.lexHead;
        type = o.type;
        aspect = o.aspect;
        foot = o.foot;
        return *this;
    }
    
    NodeIndex index;
    bool isTerminal;
    NodeOffset head;    //the root node of the segment containing this node
    NodeOffset parent;
    NodeOffset sibling; //right sibling

    //The following are not absolutely needed for a TSG
    
    NodeOffset lexHead; //the lexical head
    char type; //right now, 1 if rule has a FFT, 0 otherwise
    char aspect; //the category it comes from in a mixture model
    NodeOffset foot; //if this is zero, then it is not a TAG rule
    NodeOffset segTop; //points to the top of this node's segment

};

struct ParseTree {
public:
    ParseTree() : nodelist(NULL), markers(NULL), warps(NULL), size(0) {}
    ParseTree(TreeNode* nodelist_,bool* markers_,NodeOffset* warps_,size_t size_) : size(size_) {
        if(size > 0) {
            nodelist = new TreeNode[size];
            markers = new bool[size];
            warps = new NodeOffset[size];
            for(size_t i=0;i<size;++i) {
                nodelist[i] = nodelist_[i];
                markers[i] = markers_[i];
                warps[i] = warps_[i];
            }
        } else {
            nodelist = NULL;
            markers = NULL;
            warps = NULL;
        }
    }
    
    ~ParseTree() {
        if(nodelist != NULL)
            delete[] nodelist;
        nodelist = NULL;
        if(markers != NULL)
            delete[] markers;
        markers = NULL;
        if(warps != NULL)
            delete[] warps;
        warps = NULL;
    }
    
    ParseTree& operator=(const ParseTree& o) {
        if(nodelist != NULL)
            delete[] nodelist;
        if(markers != NULL)
            delete[] markers;
        if(warps != NULL)
            delete[] warps;
        nodelist = new TreeNode[o.size];
        markers = new bool[o.size];
        warps = new NodeOffset[o.size];
        
        for(size_t i=0;i<o.size;++i) {
            nodelist[i] = o.nodelist[i];
            markers[i] = o.markers[i];
            warps[i] = o.warps[i];
        }
        size = o.size;
        return *this;
    }

    void clearMarkers() {
        //leave the root marked
        markers[0] = true;
        for(size_t i=1;i<size;++i) {
            markers[i] = false;
        }
    }
    
    TreeNode* nodelist;
    bool* markers;
    NodeOffset* warps;
    size_t size;
};


class Segment {
public:
    Segment(ParseTree* ptree_, const NodeOffset head_);
    Segment(const Segment& o);
    Segment();
    Segment& operator=(const Segment& o);
    ~Segment();

    //TODO - make iterators warping
    class iterator {
    public:
        iterator(const Segment* const sr_, TreeNode* n_, NodeOffset offset_);
        TreeNode* operator*() {return n;}
        
        iterator& operator++();
        
        bool operator==(const Segment::iterator& o);
        bool operator!=(const Segment::iterator& o) {return !(*this == o);}

        const Segment* const sr;
        ParseTree* pt;
        TreeNode* n;
        NodeOffset offset;
        bool stub;
    private:
        std::pair<TreeNode*,NodeOffset> getNext(TreeNode* node, NodeOffset off);
        std::pair<TreeNode*,NodeOffset> getNextUp(TreeNode* node, NodeOffset off);
    };
    
    iterator begin() const;
    const iterator& end() const;
        
    size_t hashCode() const;

    bool equals(const Segment& oRef) const;
    
    ParseTree* ptree;        
    bool* markers;
    NodeOffset headIndex;
    NodeOffset* warps;

    //#ifdef TSGDEBUG
    void printMe() const {
        printf("SEGMENT PRINTOUT\n");
        for(Segment::iterator iter = begin(); iter != end(); ++iter) {
            if(!iter.stub)
                printf("NODE %d (%d) (HX %d) %d\n",iter.offset,iter.n->index,iter.n->lexHead,iter.n->type);
            else
                printf("STUB %d (HX %d) %d\n",iter.offset,iter.n->lexHead,iter.n->type);
        }
        printf("\n");
    }
    //#endif
    
private:
    iterator finish;

};

struct SegmentHash {
    size_t operator()(const Segment& sr) const {
        return sr.hashCode();
    }
};

struct SegmentEq {
    bool operator()(const Segment& a, const Segment& b) const {
        return a.equals(b);
    }
};


#endif
