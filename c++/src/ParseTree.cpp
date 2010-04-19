#include "ParseTree.hpp"

Segment::Segment(ParseTree* ptree_, const NodeOffset head_) :
    ptree(ptree_), headIndex(head_), finish(this,NULL,0) {
    
    if(ptree != NULL) {
        markers = new bool[ptree->size];
        for(size_t i=0;i<ptree->size;++i) {
            markers[i] = ptree->markers[i];
        }
    } else
        markers = NULL;
}

Segment::Segment(const Segment& o) : ptree(o.ptree), headIndex(o.headIndex), finish(this,NULL,0) {
    if(ptree != NULL) {
        markers = new bool[ptree->size];
        
        for(size_t i=0;i<ptree->size;++i) {
            markers[i] = o.markers[i];
        }
    } else
        markers = NULL;
}

Segment::Segment() : ptree(NULL), headIndex(0), finish(this,NULL,0) {
    markers = NULL;
}
    
Segment& Segment::operator=(const Segment& o) {
    ptree = o.ptree;
    headIndex = o.headIndex;
    if(ptree != NULL) {
        if(markers != NULL)
            delete[] markers;
        markers = new bool[ptree->size];
        for(size_t i=0;i<ptree->size;++i) {
            markers[i] = o.markers[i];
        }
    } else
        markers = NULL;
    return *this;
}
    
Segment::~Segment() {
    if(markers != NULL)
        delete[] markers;
    markers = NULL;
}

Segment::iterator::iterator(const Segment* const sr_, TreeNode* n_, NodeOffset offset_) :
    sr(sr_),pt(sr->ptree),n(n_), offset(offset_), stub(false) {
}
        
Segment::iterator& Segment::iterator::operator++() {
    std::pair<TreeNode*,NodeOffset> res;
    if(stub) {
        stub = false;
        res = getNextUp(n,offset);
    } else
        res = getNext(n,offset);
    
    n = res.first;
    offset = res.second;
    return *this;
}

bool Segment::iterator::operator==(const Segment::iterator& o) {
    /**
     * dont bother equating parse trees, if the
     * segment refs are equal then the parse tree
     * should be the same
     */ 
    return this->sr == o.sr &&
        this->n == o.n &&
        this->offset == o.offset;
}

std::pair<TreeNode*,NodeOffset>  Segment::iterator::getNext(TreeNode* node, NodeOffset off) {
    if(node->isTerminal) {
        if(off == sr->headIndex) //if this terminal is the head
            return std::make_pair(sr->finish.n,sr->finish.offset);
        return getNextUp(node,off);
    } else { //the next node is this node's child
        NodeOffset cOffset = off + 1;
        TreeNode& child = pt->nodelist[cOffset];
        if(sr->markers[cOffset]) {
            stub = true;
            return std::make_pair(&child,cOffset);
        } else {
            return std::make_pair(&child,cOffset);
        }
    }
}

std::pair<TreeNode*,NodeOffset> Segment::iterator::getNextUp(TreeNode* node, NodeOffset off) {
    if(off == sr->headIndex) {
        return std::make_pair(sr->finish.n,sr->finish.offset);
    }
    if(node->sibling == 0) { //no more siblings at this level
        NodeOffset pOffset = off - node->parent;
        if(pOffset == sr->headIndex) {
            return std::make_pair(sr->finish.n,sr->finish.offset);
        } else {
            return getNextUp(&(pt->nodelist[pOffset]),pOffset);
        }
    } else { //return the sibling
        NodeOffset sOffset = off + node->sibling;
        TreeNode& sibling = pt->nodelist[sOffset];
        if(sr->markers[sOffset]) {
            stub = true;
            return std::make_pair(&sibling,sOffset);
        } else
            return std::make_pair(&sibling,sOffset);
    }
}
    
Segment::iterator Segment::begin() const {
    return iterator(this,&(ptree->nodelist[headIndex]),headIndex);
}

const Segment::iterator& Segment::end() const {return finish;}
        
size_t Segment::hashCode() const {
    size_t hash = 0;
    size_t shift = 0;
    for(iterator iter = begin(); iter != end(); ++iter) {
        if(!iter.stub) {
            hash ^= (iter.n->index << shift);
            shift += 3;
            shift = shift % (sizeof(size_t) * 8);
        }
    }
    return hash;
}

bool Segment::equals(const Segment& oRef) const {
    if(ptree->nodelist == NULL || oRef.ptree->nodelist == NULL) {
        return ptree == oRef.ptree;
    }
    iterator myIter = begin();
    iterator oIter = oRef.begin();
    
    
    for(;;++oIter,++myIter) {
        //if one tree is done, they better both be done.
        if(myIter == end() || oIter == oRef.end()) {
            if(myIter == end() && oIter == oRef.end()) { 
                return true;
            } else {
                return false;
            }
        }
        
        if(myIter.stub || oIter.stub) {
            //they better both be stubs
            if(!(myIter.stub && oIter.stub))
                return false;
        } else { //neither are stubs
            if(myIter.n->index != oIter.n->index)
                return false;
        }
    }
}

