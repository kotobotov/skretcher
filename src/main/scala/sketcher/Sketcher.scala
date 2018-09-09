package sketcher
import org.scalajs.dom

import scalajs.js.annotation.JSExport
import dom.html

@JSExport
object ScratchPad extends{
  @JSExport
  def main(canvas: html.Canvas) = {
    /*setup*/
    val renderer = canvas.getContext("2d")
                   .asInstanceOf[dom.CanvasRenderingContext2D]

    canvas.width = canvas.parentElement.clientWidth
    canvas.height = canvas.parentElement.clientHeight

    renderer.fillStyle = "#f8f8f8"
    renderer.fillRect(0, 0, canvas.width, canvas.height)

    /*code*/
    renderer.fillStyle = "black"
    var down = false

    case class Pos(x:Double,y:Double)
    var point1=Pos(-1, -4)
    var point2=Pos(1, -2)
    var point3=Pos(3, 16)
    var lastSize = 1.0

    def verh=point3.y - (((point3.x*(point2.y-point1.y))+(point2.x*point1.y)-(point1.x*point2.y))/(point2.x-point1.x))
    def niz = point3.x*(point3.x-point1.x-point2.x)+(point1.x*point2.x)
    def a=verh/niz
    def b=((point2.y-point1.y)/(point2.x-point1.x))-a*(point1.x+point2.x)
    def c=(point2.x*point1.y-point1.x*point2.y)/(point2.x-point1.x)+a*point1.x*point2.x


    canvas.onmousedown =
      (e: dom.MouseEvent) => {
        point1=Pos(e.clientX, e.clientY)
        point2=Pos(e.clientX, e.clientY)
        point3=Pos(e.clientX, e.clientY)
        down = true}

    canvas.onmouseup =
      (e: dom.MouseEvent) => {
        down = false
      }
    canvas.onmousemove = {
      (e: dom.MouseEvent) =>
        point1=point2.copy()
        point2=point3.copy()
        point3=Pos(e.clientX, e.clientY)
        val diffX=math.abs(point3.x-point2.x)
        val diffY=math.abs(point3.y-point2.y)
       val distance = 26/math.sqrt(math.pow(diffX, 2)+math.pow(diffY, 2))
        val limitedDistace = if (distance>6) 6 else if (distance<0.3) 0.3 else distance
        val size = (lastSize + limitedDistace)/2.3
        lastSize=size
        val rect =
          canvas.getBoundingClientRect()
        if (down) {
          System.out.println(distance)
          if (diffX>1.5){
          val myrange = if (point2.x > point3.x) (point3.x*10).toInt to (point2.x*10).toInt else (point2.x*10).toInt to (point3.x*10).toInt
          myrange.foreach{x => renderer.fillRect(x/10, (a*x*x/100)+(b*x/10)+c, size, size)}
          } else if (diffY>1.5){
            val myrange = if (point2.y > point3.y) (point3.y).toInt to (point2.y).toInt else (point2.y).toInt to (point3.y).toInt
            myrange.foreach{y => renderer.fillRect(point3.x, y, size, size)}
          }
          renderer.fillRect(
          e.clientX - rect.left,
          e.clientY - rect.top,
            size, size
        )}
    }
  }
}