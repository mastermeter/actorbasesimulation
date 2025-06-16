import hevs.graphics.FunGraphics
import modele._

object Visu extends App {

  val w = 100
  val h = 100
  val step = 300
  val T = 20.0f
  val cellSize = 4
  val fD = 0.7f // fd utilisé pour visualiser une simulation

  val window = new FunGraphics(w * cellSize + 80, h * cellSize + 80, "Simulation Visuelle Forêt")
  var grid = GridSim.randomGrid(w, h, fD, T)

  def drawGrid(g: GridSim): Unit = {
    window.clear()
    for {
      i <- 0 until h
      j <- 0 until w
    } {
      val color = g.grid(i)(j) match {
        case Tree_Cell(_) => java.awt.Color.GREEN
        case Fire_Cell(f, _) => f match {
          case 0 => new java.awt.Color(255, 150, 0)
          case 1 => new java.awt.Color(255, 100, 0)
          case 2 => new java.awt.Color(255, 50, 0)
          case 3 => new java.awt.Color(200, 0, 0)
          case _ => new java.awt.Color(150, 0, 0)
        }
        case Empty_Cell(_) => java.awt.Color.DARK_GRAY
        case Water_Cell(_) => java.awt.Color.BLUE
      }
      window.setColor(color)
      window.drawFillRect(40 + j * cellSize, 40 + i * cellSize, cellSize, cellSize)
    }
  }

  for (_ <- 0 to step) {
    drawGrid(grid)
    Thread.sleep(50)
    grid = grid.updateGrid()
  }
}
