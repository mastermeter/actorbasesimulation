import hevs.graphics.FunGraphics
import modele._
import scala.util.Random

object SimuFG extends App{

  val step = 1000
  val w = 200
  val h = 200
  val cellSize = w/100
  val window = FunGraphics(w * cellSize, h * cellSize, "Simulation Forêt - FunGraphics")


  var grid: GridSim = GridSim.randomGrid(w, h)

  def drawGrid(g: GridSim): Unit = {
    window.clear()
    for {
      i <- 0 until h
      j <- 0 until w
    } {
      val color = g.grid(i)(j) match {
        case Tree_Cell()  => java.awt.Color.GREEN
        case Fire_Cell(a)  => java.awt.Color.RED
        case Empty_Cell() => java.awt.Color.DARK_GRAY
      }
      window.setColor(color)
      window.drawFillRect(j * cellSize, i * cellSize, cellSize, cellSize)
    }
  }
   for ( i <- 0 to step) {
     drawGrid(grid)
     grid = grid.updateGrid()
   }
}
