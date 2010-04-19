package parse

object ParseTypes {

  type Symbol = Char
  def intToSymbol(i : Int) : Char = i.toChar
  type Split = Char
  type Terminal = Int
  
  val Root : Symbol = 0.toChar
  val RootString : String = "ROOT"
  
  val Empty : Terminal = 0
  val EmptyString : String = "EPSILON"
  
  def split(s : Split) : List[Split] = {
    List((s * 2 + 1).toChar,(s * 2 + 2).toChar)
  }
  
  def isPair(a : Split, b : Split) = {
    a == b + 1 && b % 2 == 0
  }
  
  def merge(a : Split) : Split = {
    (a / 2).toChar
  }
  
  def parseSplit(s : String) : Split = {
    Integer.parseInt(s).toChar
  }
  
}
