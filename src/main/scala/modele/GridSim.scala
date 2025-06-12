package modele

import scala.util.Random

sealed trait Cell
case class Tree_Cell() extends Cell
case class Fire_Cell() extends Cell
case class Empty_Cell() extends Cell
case class Water_Cell() extends Cell

case class GridSim(w: Int, h: Int, grid: Vector[Vector[Cell]]) {

  def updateCell(i: Int, j: Int): Cell = {
    grid(i)(j) match {
      case Fire_Cell() => Empty_Cell()
      case Tree_Cell() =>
        if (hasBurningNeighbor(i, j) && Random.nextFloat()>0.5) Fire_Cell() else Tree_Cell()
      case Empty_Cell() => if (Random.nextFloat()<0.1) Tree_Cell()  else Empty_Cell()
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

  override def toString: String = {
    grid.map { row =>
      row.map {
        case Tree_Cell()  => "T"
        case Fire_Cell()  => "F"
        case Empty_Cell() => "."
      }.mkString
    }.mkString("\n")
  }
}

object GridSim {
  def randomGrid(w: Int, h: Int): GridSim = {
    def defineCell(i: Int, j: Int) : Cell = {

      if (((i == 20) && (j == 20)) || ((i == 60) && (j == 60))) Fire_Cell() else Tree_Cell()

    }
    val grid = Vector.tabulate(h, w)(defineCell)
    GridSim(w, h, grid)
  }
}
