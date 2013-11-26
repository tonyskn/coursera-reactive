package simulations

import math.random

class EpidemySimulator extends Simulator {

  def randomBelow(i: Int) = (random * i).toInt

  type Cell = (Int, Int)
  def randomFrom(l: List[Cell]): Cell = l(randomBelow(l.length))

  sealed trait Direction 
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  val directions = List(Up, Down, Left, Right)

  def neighbours(current:Cell): List[Cell] = directions map { d => {
    val (row, col) = current
    d match {
      case Up => ((row+7)%8, col)
      case Down => ((row+1)%8, col)
      case Left => (row, (col+7)%8)
      case Right => (row, (col+1)%8)
    }
  }}

  protected[simulations] object SimConfig {
    val population: Int = 300
    val roomRows: Int = 8
    val roomColumns: Int = 8

    val prevalenceRate = 0.01
    val transmissibilityPercent = 40
    val deathPercent = 25
  }

  import SimConfig._
  import scala.language.postfixOps

  val persons: List[Person] = for {
    i <- 0 until population toList
  } yield new Person(i)

  /* Infect some people */
  persons take (population * prevalenceRate).toInt foreach { _.infect }

  def personsInCell(r: Cell) = r match {
    case (row, col) => persons filter { p => p.row == row && p.col == col }
  }

  class Person (val id: Int) {
    var infected = false
    var sick = false
    var immune = false
    var dead = false

    def visiblyInfectious = sick || dead 

    // demonstrates random number generation
    var row: Int = randomBelow(roomRows)
    var col: Int = randomBelow(roomColumns)

    def cell: Cell = (row, col) 

    def infect() = {
      infected = true
      afterDelay(6)  { sick = true }
      afterDelay(14) { if (randomBelow(100) < deathPercent) { sick = false; dead = true } }
      afterDelay(16) { if (!dead) { sick = false; immune = true; } }
      afterDelay(18) { if (!dead) { infected = false; sick = false; immune = false } }
    }

    def moveTo(c: Cell) = {
      row = c._1
      col = c._2

      if (!infected && randomBelow(100) < transmissibilityPercent
        && !(personsInCell(c) filter { _.infected } isEmpty)) {
        infect
      }
    }

    def nextTick: Unit = if (!dead) {
      val cleanNeighbours = neighbours(cell) filter {
        c => personsInCell(c) filter { _.visiblyInfectious } isEmpty
      }

      if (!cleanNeighbours.isEmpty) {
        moveTo( randomFrom(cleanNeighbours) )
      }

      afterDelay(randomBelow(6))(nextTick)
    }

    afterDelay(0)(nextTick)
  }
}
