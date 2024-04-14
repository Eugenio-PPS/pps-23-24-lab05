package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics

import scala.collection.mutable
import scala.jdk.javaapi.OptionConverters
import scala.util.Random

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private object Utils:
    case class CellState(hasMine: Boolean, var clicked: Boolean)
    private def inBounds(x: Int, y: Int): Boolean =
      x >= 0 && x < size && y >= 0 && y < size
    def nearbyMines(x: Int, y: Int): Int =
      var result = 0
      for
        i <- -1 to 1
        j <- -1 to 1
      do
        if !(i == 0 && j == 0) then
          if inBounds(x + i, y + j) && minefield((x + i, y + j)).hasMine then
            result += 1

      result


  private var playerHasWon: Boolean = false
  private val minefield: mutable.Map[(Int, Int), Utils.CellState] =
    new mutable.HashMap[(Int, Int), Utils.CellState]()


  // Init minefield

  private val mineThreshold = mines.floatValue / (size * size).floatValue
  private val rng = Random()
  for
    i <- 0 until size
    j <- 0 until size
  do
    val hasMine = rng.nextFloat() <= mineThreshold
    minefield.put((i, j), new Utils.CellState(hasMine, false))

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val cell = minefield((x, y))
    cell.clicked = true
    if cell.hasMine then
      OptionToOptional(ScalaOptional.Empty()) // Option => Optional converter
    else
      playerHasWon =
        minefield.values.foldLeft(true)((allClicked, c) => if !c.hasMine then
          allClicked && c.clicked
        else
          allClicked
        )
      OptionToOptional(ScalaOptional.Just(Utils.nearbyMines(x, y)))

  def won: Boolean = playerHasWon
