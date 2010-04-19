package viz

import scala.collection.mutable.HashMap
import javax.imageio.ImageIO
import java.awt.image.{BufferedImage}
import java.awt.{Graphics2D,Font,Color,Rectangle}
import java.awt.font.{TextLayout,ImageGraphicAttribute,TextAttribute}
import java.text.{AttributedString}
import java.io.File

object FunctionDrawer {

  def main(args : Array[String]) : Unit = {
    val countMap = new HashMap[String,Tuple2[Int,Int]]()
    
    val lines = io.Source.fromFile(args(0)).getLines
    
    var maxCount = 0
    var i = 0
    lines.foreach(line => {
      val data = line.replaceAll("\n","").split("\t")
      val count = data(0).toInt
      if(count > maxCount)
        maxCount = count
      countMap += data(1).toString -> (count,i)
      i += 1
    })
    
    println("Max Count = " + maxCount)
    /**
    val colors = List(new Color(0,0,255),
                      new Color(14,32,230),
                      new Color(40,40,160),
                      new Color(50,20,190),
                      new Color(20,70,190),
                      new Color(70,20,180),
                      new Color(100,100,130),
                      new Color(50,20,100))
*/  

    val colors = List(new Color(0,200,0),
                      new Color(14,234,23),
                      new Color(40,140,16),
                      new Color(50,120,19),
                      new Color(20,170,19),
                      new Color(70,120,18),
                      new Color(10,100,13),
                      new Color(50,120,10))
/**
    val colors = List(new Color(24,24,24),
                      new Color(52,52,52),
                      new Color(83,83,83),
                      new Color(102,102,102),
                      new Color(120,120,120),
                      new Color(159,159,159),
                      new Color(240,240,240),
                      new Color(200,200,200))
   */ 
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
          
          y = rando.nextGaussian(y,(maxCount - count).toFloat * 10).toFloat
          
          /**
           *  from bottom
           */ 
          y = height - y
/**
          //from middle
          y /= 2
          if(rando.nextBoolean())
            y *= -1
          y += height / 2
*/
          

          //println(y)

          //println(str)
          //println(x + " " + y)
          tl.draw(vIg,x,y.toInt)
                  
        }
      }
    })

    ImageIO.write(vIm,"png",new File(args(1)))
  }

}
