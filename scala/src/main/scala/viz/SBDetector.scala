package viz

import parse._
import corpora.treebank._
import scala.collection.mutable.HashMap
import javax.imageio.ImageIO
import java.awt.image.{BufferedImage}
import java.awt.{Graphics2D,Font,Color,Rectangle}
import java.awt.font.{TextLayout,ImageGraphicAttribute,TextAttribute}
import java.text.{AttributedString}
import java.io.File
import java.util.regex.{Pattern,Matcher}
import java.io.{BufferedWriter,FileWriter,File}

object SBDetector {

  def main(args : Array[String]) : Unit = {

    val pcfg = new PCFG
	var data = TreebankData.read(args(0),pcfg)

    //data = data.slice(0,5)

    def getTerminalYield(nt : NonTerminalNode) : String = {
      val terminals = (new ParseTree(nt)).terminals.map(pcfg.getTerm(_))
      ("" /: terminals)(_ + " " + _)
    }

    var terms : List[String]= data.map(t => getTerminalYield(t.root))

    

    println("Removing spaces around punctuation")
    
    val punc = "[\\.!\\?]"
    terms = terms.map(s => {
      var pat = Pattern.compile("(.*[^\\s])\\s*(" + punc + ")$")

      var ret = s.replaceAll("-LRB- ","(").replaceAll(" -RRB-",")")
      ret = ret.replaceAll(" ,",",").replaceAll(" 's","'s")
      ret = ret.replaceAll(" ,",",").replaceAll(" 's","'s")
      var mat = pat.matcher(ret)
      while(mat.find()) {
        //println("RMPUNC " + mat.group(0) + "---" + mat.group(1) + "---" + mat.group(2))
        ret = mat.group(1) + mat.group(2)
        //println(ret)
      }
      ret
    })

    //terms.foreach(s => println(s))
    
    println("Counting errors")
                                                                     
    val fp = new HashMap[String,Int]()
    val tn = new HashMap[String,Int]()
    val window = 4
    var mask = ("" /: (for{i <- 1 to window} yield ".?").toList)(_ + _)

    terms.foreach(line => {
      val pat = Pattern.compile("(" + mask + punc + ")\\s" + mask)
      val mat = pat.matcher(line)
      val len = line.length
      var start = 0
      while(mat.find(start)) {
        val s = mat.group(0)
        if(mat.end < line.length) {
          //println("found " + s)
          val c = tn.getOrElse(s,0) + 1
          tn += s -> c

        }
        start = mat.start + mat.group(1).length
      }

      val endS = line.substring(len - 1,len).replaceAll(punc,"")
      println("!!" + endS)
      if(endS.length > 0) {

        val st = if(len - window > 0) (len - window) else 0
        val s = line.substring(st,len)
        println(s)
        fp += s -> (tn.getOrElse(s,0) + 1)
      }
    })
  

    def writeData(s : String, hm : HashMap[String,Int]) = {

      val fname = args(1) + "." + s
      println("writing " + s + " yields to " + fname)

      val bw = new BufferedWriter(
        new FileWriter(new File(fname)))
      val sorted = hm.elements.toList.sort((a,b) => {a._2 > b._2})
      sorted.foreach(el => {
        bw.write(el._2.toString + "\t" + el._1 + "\n")
      })
      bw.close()
    }

    writeData("fp",fp)
    writeData("tn",tn)

   
/** 
    val width : Int = 1000
    val height : Int = (1.6180339077 * width).toInt
  
    val vIm = new BufferedImage(width, height, BufferedImage.TYPE_INT_RGB)
    val vIg : Graphics2D = vIm.createGraphics()

    vIg.setColor(Color.white)
    vIg.fill(new Rectangle(0,0,width,height))


    val fHeight = height / 100


    println("Height = " + fHeight)
    val f = new Font("Kacst",Font.BOLD,fHeight)
    
    val frc = vIg.getFontRenderContext()

    import scala.util.Random
    
    val rando =  butil.Util.malletRandom 

    countMap.elements.foreach(_ match {
      case (str,(count,index)) => {
        //if(index % 10 == 0) {
        if(true){
          vIg.setColor(colors(new Random().nextInt(colors.length)))
          val tl = new TextLayout(str,f,frc)
          //puts the most frequent at top (0)
          //the other height is 
          var maxX = width - (str.length() * fHeight)
          if(maxX < 0) 
            maxX = 1
          var x = new Random().nextInt(maxX)
          //y goes from 0 to height - fHeight
          //0 -> count 1
          //height - fHeight -> maxCount
          val ratio = (count.toFloat - 1) / maxCount.toFloat
          var y : Float = (height - fHeight) * Math.pow(ratio,.05).toFloat
          //println(y)
          

     //     y = height - y

          //from middle
          y /= 2
          if(rando.nextBoolean())
            y *= -1
          y += height / 2

          

          //println(y)

          //println(str)
          //println(x + " " + y)
          tl.draw(vIg,x,y.toInt)
                  
        }
      }
    })

    ImageIO.write(vIm,"png",new File(args(1)))
  }
*/
  }
}
