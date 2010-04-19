package parse.tsg.parse

class TSGParser {

  /**
   * TODO : Unking! 
   * 
   * Full Train Pipeline - Unk trees, get segments
   * 
   * To parse a sentence, follow Johnson2007 - with a binaraized pcfg he 
   * does basic generation but with the added element of the proability distribution 
   * where each rule is split at a certain index, so P(A->BC,8,4) would mean using A->BC 
   * and B spans 4 C spans 4.  It is not clear wether or not they use another conditioning
   * factor which is the start index of the span...doing so would make the data even more sparse
   * 
   * These probabilities are obtained by the trees created during training.  We get from them
   * the MAP grammar (does he mean a TSG or PCFG?)
   * 
   * We sample several times (this is a parameter) from the MAP Grammar.  The first generation
   * is automatically accepted, I guess.  The generation is more like top down parsing, but
   * using a randomized choice of rules rather than the best rule.
   * 
   * One Idea - since hopefully our proposal distribution is close to our true distribution, 
   * 	starting with the best top down parse might be a good idea, or maybe scoring the first N parses
   * 	and using the top scoring one in the true distribution as the starting point is a good idea.
   * 
   * Once a sentence has been top down parsed several times, it seems like (this is the Monte Carlo
   * Integral part) we just pick the tree which got the most votes (iterations).  This perhaps
   * doesnt take into account the line where they say the second step is to get the CFG tree.  If they
   * used a regular PCFG for parsing, then this might mean that after the PCFG generates the tree,
   * we find all TSG parses of that tree and each gets a vote.
   * 
   * TwoIdea - Why not use the berkeley parser to generate trees?  If all we need is a proposal
   * 	distribution, then we can use any parses we want.  Additionally, we can always get a quick 
   * 	guess at the segment structure of a sentence and parse simply by finding all TSG parses of a 
   * 	pcfg parse.  This would act as a reranker.
   * 
   * 	For FFTags, we could first parse normally but apply segments which are trained on
   * 	trees with ff tags - so that and TSG parse implies a FF Tag assignment.  For Semantic Role Labeling
   * 	segments which contain an arg as a head or only the root and main verb could be used, then 
   * 	you overlay the segments and get argument labels.
   */  
  
}
