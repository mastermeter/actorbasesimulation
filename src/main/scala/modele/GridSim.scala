package modele

import scala.util.Random

sealed trait Cell
case class Tree_Cell() extends Cell
case class Fire_Cell(firepower : Int) extends Cell
case class Empty_Cell() extends Cell
case class Water_Cell() extends Cell

case class GridSim(w: Int, h: Int, grid: Vector[Vector[Cell]]) {

  def updateCell(i: Int, j: Int): Cell = {
    grid(i)(j) match {
      case Fire_Cell(a) => if (a > 5) Empty_Cell() else Fire_Cell(a+1)
      case Tree_Cell() =>
        if (hasBurningNeighbor(i, j) && Random.nextFloat()>0.5) Fire_Cell(0) else Tree_Cell()
      case Empty_Cell() => Empty_Cell()
      case Water_Cell() => Water_Cell()
    }
  }

  def hasBurningNeighbor(i: Int, j: Int): Boolean = {
    val directions = for {
      di <- -1 to 1
      dj <- -1 to 1
      if !(di == 0 && dj == 0)
    } yield (i + di, j + dj)

    directions.exists {
      case (ni, nj) =>
        ni >= 0 && ni < h && nj >= 0 && nj < w && grid(ni)(nj).isInstanceOf[Fire_Cell]
    }
  }

  def updateGrid(): GridSim = {
    val newGrid = Vector.tabulate(h, w)((i, j) => updateCell(i, j))
    GridSim(w, h, newGrid)
  }

  /*override def toString: String = {
    grid.map { row =>
      row.map {
        case Tree_Cell()  => "T"
        case Fire_Cell(a)  => "F"
        case Empty_Cell() => "."
      }.mkString
    }.mkString("\n")
  }*/
}

object GridSim {
  def randomGrid(w: Int, h: Int): GridSim = {
    def defineCell(i: Int, j: Int) : Cell = {

      val randX : Int = Random.nextInt(w)
      val randY : Int = Random.nextInt(h)

      if ((i == randX) && (j == randY)) Fire_Cell(0) else Tree_Cell()

    }
    val grid = Vector.tabulate(h, w)(defineCell)
    GridSim(w, h, grid)
  }
}
