#ifndef TSG_PARSETREE
#define TSG_PARSETREE 1

#include <utility>
#include <stdio.h> //for printf

typedef int NodeIndex; //max rule index is 31006, but give it lots of room
typedef unsigned char NodeOffset; //maximum treebank tree size is 395, making max nts < 256

struct TreeNode {
    TreeNode() {}
    TreeNode(NodeIndex index_,
             bool isTerminal_,
             NodeOffset head_,
             NodeOffset parent_,
             NodeOffset sibling_,
             NodeOffset lexHead_) :
        index(index_), isTerminal(isTerminal_),
        head(head_), parent(parent_), sibling(sibling_),
        lexHead(lexHead_) {};

    TreeNode& operator=(const TreeNode& o) {
        index = o.index;
        isTerminal = o.isTerminal;
        head = o.head;
        parent = o.parent;
        sibling = o.sibling;
        lexHead = o.lexHead;
        return *this;
    }
    
    NodeIndex index;
    bool isTerminal;
    NodeOffset head;
    NodeOffset parent;
    NodeOffset sibling;
    NodeOffset lexHead;
};

struct ParseTree {
public:
    ParseTree() : nodelist(NULL), markers(NULL), size(0) {}
    ParseTree(TreeNode* nodelist_,bool* markers_,size_t size_) : size(size_) {
        if(size > 0) {
            nodelist = new TreeNode[size];
            markers = new bool[size];
            for(size_t i=0;i<size;++i) {
                nodelist[i] = nodelist_[i];
                markers[i] = markers_[i];
            }
        } else {
            nodelist = NULL;
            markers = NULL;
        }
    }
    ~ParseTree() {
        if(nodelist != NULL)
            delete[] nodelist;
        nodelist = NULL;
        if(markers != NULL)
            delete[] markers;
        markers = NULL;
    }
    ParseTree& operator=(const ParseTree& o) {
        if(nodelist != NULL)
            delete[] nodelist;
        if(markers != NULL)
            delete[] markers;
        nodelist = new TreeNode[o.size];
        markers = new bool[o.size];
        
        for(size_t i=0;i<o.size;++i) {
            nodelist[i] = o.nodelist[i];
            markers[i] = o.markers[i];
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
    size_t size;
};



class Segment {
public:
    Segment(ParseTree* ptree_, const NodeOffset head_);
    Segment(const Segment& o);
    Segment();
    Segment& operator=(const Segment& o);
    ~Segment();
    
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


    //Kill me eventually
    void printMe() const {
        printf("SEGMENT PRINTOUT\n");
        for(Segment::iterator iter = begin(); iter != end(); ++iter) {
            if(!iter.stub)
                printf("NODE %d (%d)\n",iter.offset,iter.n->index);
            else
                printf("STUB %d\n",iter.offset);
        }
        printf("\n");
    }

    
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
