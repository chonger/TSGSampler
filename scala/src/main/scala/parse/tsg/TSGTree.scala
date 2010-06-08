package parse.tsg


class SegTree(val segment : ParseTree, 
              val children : List[SegTree]) {

  def print(pcfg : PCFG) : Unit = {
    
    println(PCFGPrinter.treeToString(pcfg,segment))

    children.map(_.print(pcfg))
    
  }

}
