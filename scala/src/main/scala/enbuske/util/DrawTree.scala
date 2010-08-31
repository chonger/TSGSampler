package enbuske.util

import processing.core.PApplet
import parse._
import parse.tsg._
import rita.RiText

class TreeBankSet() {
  def get(applet : PApplet, filename : String) = {
    var pcfg = new DirectlyEstimatedPCFG()
    var trees = pcfg.read(filename)
    new TreeSet(applet,trees,pcfg)
  }

}

class ChooseSet() {
  def get(applet : PApplet, bank : String, packed : String, targ : String, goldF : String) = {
    val pcfg = new DirectlyEstimatedPCFG()
    val raw = pcfg.read(bank)
    val targD = pcfg.read(targ)
    val gold = pcfg.read(goldF)
    pcfg.process(raw ::: targD)
    val (syms,terms) = pcfg.size
    
    println("PCFG created with " + syms + " symbols and " + terms + " terminals")
    val rando = new java.util.Random()
    val data = raw.map(r => new ParseTree(r.root) with Markers)
  
    val packer = new TSGPackager(pcfg)
    val ptsg = packer.unpack(data.toArray,packed)
    ptsg.addPCFGRules(targD)
                             
   //val sorted = ptsg.counts.elements.toList.sort((a,b) => {a._2 > b._2}).map(_._1)

    new ChoiceTreeSet(applet,targD,ptsg,gold)
  }

}

class PTSGSet(applet : PApplet, bank : String, packed : String, targ : String) {

  val pcfg = new DirectlyEstimatedPCFG()
  val raw = pcfg.read(bank)
  val targD = pcfg.read(targ)
  pcfg.process(raw ::: targD)
  val (syms,terms) = pcfg.size
  
  println("PCFG created with " + syms + " symbols and " + terms + " terminals")
  val rando = new java.util.Random()
  val data = raw.map(r => new ParseTree(r.root) with Markers)
  
  val packer = new TSGPackager(pcfg)
  val ptsg = packer.unpack(data.toArray,packed)
  val sorted = ptsg.counts.elements.toList.sort((a,b) => {a._2 > b._2}).map(_._1)
  val mainTS = new TreeSet(applet,targD.filter(_.nonterminals.length < 130),ptsg.pcfg)
  var fragTS = (for(i <- 1 to 6) yield {
    val ret = new TreeSet(applet,sorted,ptsg.pcfg)
    pickFrag(ret)
    ret
  }).toList
  fragTS.foreach(f => flyOut(f))

  mainTS.doneWithChange = () => {
    fragTS.foreach(f => flyOut(f))
  }
  
  def flyOut(frag : TreeSet) : Unit = {
    def move() : Unit = {
      if(frag.rootPos._3 > 500) {
        pickFrag(frag)
        flyOut(frag)
      } else {
        frag.rootPos = (frag.rootPos._1,frag.rootPos._2,frag.rootPos._3 + 5)
        frag.setTimer(move,50)
      }
    }
    frag.rootPos = (frag.rootPos._1,frag.rootPos._2,-500)
    fragTS = fragTS.sort((a,b) => {a.rootPos._3 < b.rootPos._3})
    frag.setTimer(move,rando.nextInt(10000))
  }


  def pickFrag(frag : TreeSet) = {
    val tree = new ParseTree(mainTS.curTree.n.asInstanceOf[NonTerminalNode])
    var cont = true
    while(cont) {
      val r = tree.nonterminals(rando.nextInt(tree.nonterminals.length))
      val segs : List[ParseTree] = Nil //ptsg.getTiles(r) FIX ME!!!!
      if(segs.length > 0) {

        val scr = segs zip segs.map(x => Math.pow(ptsg.scoreMap(x),.5) )
        val tot = (0.0 /: scr)((a,b) => a + b._2)
        val ran = rando.nextDouble() * tot

        var s : ParseTree = null
        var con2 = true
        var sum = 0.0

        scr.foreach(sc => {
          if(con2) {
            if(sc._2 + sum > ran) {
              con2 = false
              s = sc._1
            } else {
              sum += sc._2

            }
          }
        })


        frag.changeTreeFrag(s)

        def setPos(n : DrawNode,root : DrawNode,pos : Tuple2[Float,Float]) : Unit = {
          n.dx = pos._1
          n.dy = pos._2
          if(n.children.length > 0) {
            n.children zip root.children foreach(c => {
              setPos(c._1,c._2,(c._2.dx,c._2.dy))
            })
          }
        }

        def findPos(n : NonTerminalNode,root : DrawNode,pos : Tuple2[Float,Float]) : Unit = {
          if(n eq root.n) {
            frag.rootPos = (pos._1,pos._2,140)
            setPos(frag.curTree,root,(0,0))
          } else {
            root.children.foreach(c => {
              findPos(n,c,(pos._1 + c.dx,pos._2 + c.dy))
            })
          }
        }

        findPos(r,mainTS.curTree,(mainTS.rootPos._1,mainTS.rootPos._2))
        cont = false
      }
    }
  }


  def draw(x : Float, y : Float, z : Float, rx : Float, ry : Float) = {
   
    mainTS.checkTimer()

    applet.translate(applet.width/2,applet.height/2,0)
    applet.translate(-x,-y,-z)
    
    applet.rotateX(rx)
	applet.rotateY(ry)
    
    mainTS.draw(0,0,0,0,0)

    if(!mainTS.tmrOn)
      fragTS.foreach(_.draw(0,0,0,0,0))

    1
  
  }

  def changeTree(delta : Int) = {
    mainTS.changeTree(delta)
    allNewFrags()
  }

  def changeTreeAnimated(delta : Int) : Unit = {
    mainTS.changeTreeAnimated(delta)
    allNewFrags()
  }

  def allNewFrags() = {
    fragTS.foreach(a => pickFrag(a))
  }

  def setCycle(c : Boolean) = {
    
  }

  def cycle = false
  
}

object TreeConst {
 
  val elSz = 120
  val gap = 80
}

class ChoiceTreeSet(applet : PApplet, 
                    trees : List[ParseTree], 
                    val ptsg : PTSG,
                    val gold : List[ParseTree]) extends TreeSet(applet,trees,ptsg.pcfg) { 

  var selNode : DrawNode = null
  var selParent : DrawNode = null
  var path : List[Tuple2[DrawNode,Int]] = Nil
  var selInd : Int = 0
  import scala.collection.mutable.HashMap

  var goldMap = new HashMap[ParseTree,ParseTree]() 
  var gNodeMap = new HashMap[RefWrapper,ParseTypes.Symbol]()
  trees zip gold foreach(a => goldMap += a._1 -> a._2)

  var taggings : HashMap[RefWrapper,List[Tuple2[ParseTypes.Symbol,Double]]] = null
  var disp : String = "BEN"

  val infofont = applet.loadFont("Type20.vlw")

  changeTree(0) //call it again

  def getTags(dnode : DrawNode) = {
    var lst = taggings(new RefWrapper(dnode.n.asInstanceOf[NonTerminalNode]))

    lst = lst.sort((a,b) => {a._2 > b._2})

    var tot = (0.0 /: lst)(_ + _._2)
    
    val lstr : List[Tuple2[ParseTypes.Symbol,Int]]= lst.map(a => (a._1,(a._2 / tot * 10000).toInt))
    lstr.map(a => (a._1,(a._2.toFloat / 100)))
  }

  def select(dnode: DrawNode) {
    selNode.selected = true
    
    val lste = getTags(dnode)

    disp = "GOLD TAG - " + pcfg.symbolStrings(gNodeMap(new RefWrapper(dnode.n.asInstanceOf[NonTerminalNode]))) + "\n"
    disp += ("" /: lste)((a,b) => {
      a + pcfg.symbolStrings(b._1) + " --- " + b._2 + "%\n"
    })
    
    
  }

  def allDNodes(n : DrawNode) : List[DrawNode] = {
    n :: n.children.flatMap(c => {
      allDNodes(c)
    })
  }

  override def changeTreeTo(tree : ParseTree) = {
    super.changeTreeTo(tree)
    taggings = null //ptsg.alltagNodes(tree) //FIX ME!!!!!!
    selNode = curTree
    selParent = null
    selInd = 0
    path = Nil

    gNodeMap.clear()
    val gTree = goldMap(tree)
    tree.nonterminals zip gTree.nonterminals foreach (a => {
      gNodeMap += new RefWrapper(a._1) -> a._2.symbol
    })

    val dnodes = allDNodes(curTree)
    
    dnodes.foreach(d => {
      d.n match {
        case in : InternalNode => {
          val gT = gNodeMap(new RefWrapper(in))
          val tags = getTags(d)
          if(pcfg.symbolStrings(gT) != pcfg.symbolStrings(tags(0)._1))
            d.wrongTag = true
          if(tags(0)._2 < 80)
            d.closeCall = true
        }
        case _ => {
          //do nothing
        }
      }
    })

    select(selNode)
  }

  def goDown() = {
    if((selNode.children.length > 1) || (selNode.children.length == 1 && selNode.children(0).children.length > 0)) {
      path ::= (selParent,selInd)
      selInd = 0
      selParent = selNode
      selNode.selected = false
      selNode = selNode.children(0)
      select(selNode)
    }
  }

  def goUp() = {

    if(path.length > 0) {
      var pop = path(0)
      path = path.drop(1)
      selNode.selected = false
      selNode = selParent
      selParent = pop._1
      selInd = pop._2
      select(selNode)
    }
    
  }
  
  def goRight() = {
    if(selParent != null) {
      if(selInd < selParent.children.length - 1) {
        selInd += 1
        selNode.selected = false
        selNode = selParent.children(selInd)
        select(selNode)
      }
    }
  }

  def goLeft() = {
    if(selParent != null) {
      if(selInd > 0) {
        selInd -= 1 
        selNode.selected = false
        selNode = selParent.children(selInd)
        select(selNode)
      }
    }
  }

  def drawo(x : Float, y : Float, z : Float, rx : Float, ry : Float) = {
   
    checkTimer()
    
    applet.pushMatrix()
    applet.translate(applet.width/2,applet.height/2,0)
    applet.translate(-x,-y,-z)
    
    applet.rotateX(rx)
	applet.rotateY(ry)
    
    draw(0,0,0,0,0)

    applet.popMatrix()

    applet.fill(255)
    applet.textFont(infofont)
    applet.text(disp,20,200)

    1
  
  }
}


class TreeSet(val applet : PApplet, val trees : List[ParseTree], val pcfg : PCFG) {

  val rando = new java.util.Random()

  var rootPos = (0f,-300f,0f)

  var curTree : DrawNode = null
  var treeIndex = 0

  val font = applet.loadFont("Dunhill48.vlw")

  //changeTree(0)

  var cycle = false
  var freemove = false
  var rx : Float = 0
  var ry : Float = 0

  def changeTreeTo(tree : ParseTree) : Unit = {
    curTree = makeD(tree.root)
    
    def setDep(n : DrawNode, d : Int) : Unit = {
      n.depth = d
      n.children.foreach(k => setDep(k,d + 1))
    }
    setDep(curTree,0)

    def setH(n : DrawNode) : Int = {
      n.height = 0
      n.children.foreach(k => {
        val h = setH(k) + 1
        if(h > n.height)
          n.height = h
      })
      n.height
    }
    setH(curTree)

    
  }

  def changeTreeFrag(tree : ParseTree) = {
    curTree = makeD(tree.root)
    
    def setFrag(d : DrawNode) : Unit = {
      d.asFrag = true
      d.children.foreach(a => setFrag(a))
    }

    setFrag(curTree)

  }

  def changeTree(delta : Int) = {
    var newInd = treeIndex + delta
    if(newInd < 0)
      newInd = trees.length - 1 + newInd
    if(newInd > trees.length - 1)
      newInd -= (trees.length - 1)
    

    changeTreeTo(trees(newInd))

    /**
    def depth(n : DrawNode) : Float = {
      if(n.children.length == 0) {
          n.dy
      } else {
        val kvals = n.children.map(a => depth(a) + a.dy)
        (0f /: kvals)((a,b) => if(b > a) b else a) 
      }
    }

    val dep = depth(curTree)
    */
    //pos = (0,0,pos._3)
    treeIndex = newInd
  }  

  var tmr = 0
  var tmrMax = 0f
  var tmrOn = false
  var tmrAc : () => Unit = () => {0}
  def setTimer(f : () => Unit, secs : Float) {
    tmr = applet.millis()
    tmrOn = true
    tmrMax = secs
    tmrAc = f
  }
  def checkTimer() = {
    if(tmrOn && applet.millis() - tmr > tmrMax) {
      tmrOn = false
      tmrAc()
    }
  }

  var doneWithChange : () => Unit = () => {

  }
  

  def changeTreeAnimated(delta : Int) : Unit = {

    def grow() : Unit = {
      
      def getleaf(n : DrawNode) : List[DrawNode] = {
          if(!n.show) {
            List(n)
          } else {
            n.children.flatMap(l => getleaf(l))
          }
      }

      val leaves = getleaf(curTree)

      if(leaves.length == 0) {
        if(cycle)
          setTimer(() => {changeTreeAnimated(delta)},10000)

        doneWithChange()

      } else {
        println("GROW")
        val ind = rando.nextInt(leaves.length)
        leaves(ind).show = true
        setTimer(grow,20 + rando.nextInt(100))
      }

    }

    def f() : Unit = {
      if(curTree.children.filter(_.show).length == 0) {
        setTimer(() => {
          println("NEW TREE!")
          changeTree(delta)
          curTree.setshow(false)
          curTree.show = true
          grow()
        },1000)
      } else {
        println("DELETE");
        //find a leaf and remove it

        def getleaf(n : DrawNode) : List[DrawNode] = {
          val sk = n.children.filter(_.show)
          if(sk.length == 0) {
            List(n)
          } else
            n.children.filter(_.show).flatMap(l => getleaf(l))
        }

        val leaves = getleaf(curTree)

        val ind = rando.nextInt(leaves.length)
        leaves(ind).show = false

        setTimer(f,20 + rando.nextInt(100))
      }
    }
    
    f()
  }  
  
/**
  def mouseDragged() = {
    val r = .001
    if(freemove) {
      rx += ((applet.pmouseY - applet.mouseY) * r).toFloat
      ry += ((applet.mouseX - applet.pmouseX) * r).toFloat
    }
  }
  * def setFreeMove(on_? : Boolean) = {
    freemove = on_?
  }
  *
  * 
*/

  def setCycle(on_? : Boolean) = {
    cycle = on_?
  }

  def draw(x : Float, y : Float, z : Float, rx : Float, ry : Float) = {
    applet.pushMatrix()

    checkTimer()

    applet.textFont(font)
    applet.fill(0,255,0)
    applet.strokeWeight(4)
    applet.stroke(255)
    //applet.translate(applet.width/2,applet.height/2,0)
    //applet.translate(-x,-y,-z)

    applet.translate(rootPos._1,rootPos._2,rootPos._3)

    //applet.rotateX(rx)
	//applet.rotateY(ry)
    curTree.drawBG(curTree.height)
    curTree.draw(curTree.height,rootPos._3)
    applet.popMatrix()
    1
  }


  def makeD(node : TreeNode) : DrawNode = {

    val ret = node match {
      case tn : TerminalNode => 
        new DrawNode(tn,Nil,pcfg,applet)
      case pt : PreTerminalNode => 
        new DrawNode(pt,List(makeD(pt.kid)),pcfg,applet)
      case un : UnderspecifiedNode => 
        new DrawNode(un,Nil,pcfg,applet)                     
      case in : InternalNode => 
        new DrawNode(in,in.children.map(n => makeD(n)),pcfg,applet)                     
    }    

    //determine xy of children
    if(ret.children.length > 0) {

      val len = (0f /: ret.children)((tot,n) => {
        tot + n.sdim._1
      })


      
      var xoff = - len / 2 
      
      ret.children.foreach(kid => {

        //set dx and dy        
        
        //if(ret.children.length == 1)
        //  kid.dx = 0
        //else
        
        xoff += kid.sdim._1 / 2 

        kid.dx = xoff

        kid.dy = kid.sdim._2 + TreeConst.gap
        
        xoff += kid.sdim._1 / 2
      })
    }
    ret
  }
  
  


}

class DrawNode(val n : TreeNode, var children : List[DrawNode], pcfg : PCFG, val applet : PApplet) {

  var wrongTag : Boolean = false
  var closeCall : Boolean = false

  var selected : Boolean = false
  var asFrag = false
  
  def getDim(str : String) = {
      RiText.setDefaultFont("Dunhill48.vlw")
      val rt = new RiText(applet,str,0,0)
      val wid = rt.getBoundingBox().getWidth()
      val hgt = rt.getBoundingBox().getHeight()
      rt.dispose()
      (wid.toFloat,hgt.toFloat)
  }

    val nstr = n match {
      case tn : TerminalNode => pcfg.getTerm(tn)
      case n : NonTerminalNode => pcfg.getSym(n)
    }
    var ndim : Tuple2[Float,Float] = getDim(nstr)
    var sdim : Tuple2[Float,Float] = if(ndim._1 < TreeConst.elSz) {
      (TreeConst.elSz,ndim._1)
    } else {
      ndim
    }

    val klen = (0f /: children)(_ + _.sdim._1)
    if(sdim._1 < klen) 
      sdim = (klen,sdim._2)

    var dx : Float = 0
    var dy : Float = 0

    var show = true

    var depth :Int =  0
    var height :Int = 0

    def setshow(b : Boolean) {
      show = b
      children.foreach(_.setshow(b))
    }    

    def drawBG(curH : Float) : Unit = {
      val row = curH - depth
      val prc = depth.toFloat / curH.toFloat
      
      val c1 = (250f,0f,200f)
      val c2 = (0f,0f,250f)
      val cv = (c2._1 - c1._1,c2._2 - c1._2,c2._3 - c1._3)
            
      applet.stroke(c1._1 + cv._1 * prc,c1._2 + cv._2 * prc, c1._3 + cv._3 * prc)
      applet.fill(c2._1 - cv._1 * prc,c2._2 - cv._2 * prc, c2._3 - cv._3 * prc)
        

      applet.pushMatrix()
      val step = 65
      applet.scale(.7f)
      applet.translate(0,0,-row * step - 500)
      applet.rotateX(scala.Math.Pi.toFloat * prc / 2)
      applet.rect(-sdim._1 /2,-sdim._2 /2,sdim._1,sdim._2 * (curH - depth + 2))
      applet.popMatrix()
      
      if(n.isInstanceOf[InternalNode]) {
        children.foreach(dnode => {
          if(dnode.show) {
            applet.pushMatrix()
            
            applet.translate(dnode.dx,dnode.dy,0)
              
            dnode.drawBG(curH)
            
            applet.popMatrix()
          }  
        })
      }

    }

    def draw(curH : Float, curZ : Float) : Unit = {
      
      val uspec = n.isInstanceOf[UnderspecifiedNode]

      val factor : Float = (1f - (Math.abs(curZ) / 500.toFloat)) * 255f

      if(uspec) {
        applet.fill(255,255,255)
        applet.stroke(0,100,30)
      } else {
        applet.fill(50,20,20)
        applet.stroke(80,30,30)
      }

      if(asFrag) {
        if(uspec) {
          applet.fill(255,200,200,factor)
          applet.stroke(255,230,230,factor)
        } else {
          applet.fill(220,230,255,factor)
          applet.stroke(100,100,140,factor)
        }
      }

      applet.pushMatrix()      
      applet.translate(0,0,-10)

      
      applet.ellipse(0,0,TreeConst.elSz,TreeConst.elSz)

      if(selected) {
        applet.pushStyle()
        applet.fill(0,0)
        applet.stroke(255)
        applet.strokeWeight(20)
        applet.ellipse(0,0,TreeConst.elSz + 20,TreeConst.elSz + 20)
        applet.popStyle()
      }

      applet.popMatrix()
      
      if(wrongTag) {
        applet.fill(255,0,0)
        applet.stroke(0)
        applet.strokeWeight(2)
        applet.ellipse(-TreeConst.elSz/2,-TreeConst.elSz/2,30,30)
      }

      if(closeCall) {
        applet.fill(255,255,0)
        applet.stroke(0)
        applet.strokeWeight(2)
        applet.ellipse(-TreeConst.elSz/2,TreeConst.elSz/2,30,30)
      }

      if(uspec)
        applet.fill(140,140,140)
      else
        applet.fill(0,255,0)
      if(asFrag) {
        if(uspec)
          applet.fill(200,10,10,factor)
        else
          applet.fill(10,40,145,factor)
      }
      
      //these divisions could be removed and the one below
      applet.text(nstr,-ndim._1 / 2,ndim._2 / 2)

      applet.strokeWeight(4)
      applet.stroke(100,100,100,factor)

      n match {
        case pt : PreTerminalNode => {
          if(children.length > 0) {
            val kid = children(0)
            if(kid.show) {
              applet.line(0,5,-20,kid.dx,kid.dy,-20)
              val term = kid.nstr
              val dimt = kid.ndim

              /**
              applet.pushMatrix() 
              applet.translate(kid.dx,kid.dy,-5)
              applet.fill(0)
              applet.stroke(255)
              applet.scale(1.4f)
              applet.rect(-dimt._1/2,-dimt._2/2,dimt._1,dimt._2)
              applet.popMatrix()
              */

              if(asFrag) {
                applet.fill(255,255,255,factor)
              } else {
                applet.fill(255,255,255)
              }
              applet.text(term,kid.dx - dimt._1 / 2,kid.dy +  dimt._2/2)


            }
          }
        }
        case un : UnderspecifiedNode => {
          //do nothing
        }
        case in : InternalNode => {          

          children.foreach(dnode => {
            if(dnode.show) {
              applet.pushMatrix()
              
              applet.line(0,0,-20,dnode.dx,dnode.dy,-20)
              
              applet.translate(dnode.dx,dnode.dy,0)
              
              dnode.draw(curH,curZ)
              
              applet.popMatrix()
            }

          })
        }

      }
  
    }
  }


