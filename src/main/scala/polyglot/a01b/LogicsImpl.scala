package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import util.Sequences.Sequence
import util.Sequences.Sequence.*
import scala.util.Random

// ***** UTILITY EXTENSIONS *****
extension [E] (s: Sequence[E])
  def length() : Int = s match
    case Cons(h, t) => 1 + t.length()
    case _ => 0

  def foreach(f: E => Unit): Unit = s match
    case Cons(h, t) => f(h); t.foreach(f)
    case _ => ()

  def updateWhere(predicate: E => Boolean, updatingFunc: E => E): Sequence[E] = s match
    case Cons(h, t) => Cons(if predicate(h) then updatingFunc(h) else h, t.updateWhere(predicate, updatingFunc))
    case _ => Nil()

// ***** UTILITY DATA STRUCTURES *****

enum Cell:
  case MineCell()
  case EmptyCell(adjacentMines: Int)

case class Coordinates(x: Int, y: Int)

// ***** ACTUAL LOGICS IMPLEMENTATION *****

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl (private val size: Int, private val mineCoordinates: Sequence[Coordinates]) extends Logics:

  private var cells: Sequence[(Coordinates, Cell)] = Sequence()

  initCells()
  private def initCells(): Unit =
    for
      x <- 0 until size
      y <- 0 until size
    do
      val coordinates = Coordinates(x, y)
      if !mineCoordinates.contains(coordinates) then
        cells = cells.concat(Sequence((coordinates, Cell.EmptyCell(0))))

    mineCoordinates.foreach(
      mineCoordinates =>
        cells = cells.concat(Sequence((mineCoordinates, Cell.MineCell())))
        for
          x <- mineCoordinates.x - 1 to mineCoordinates.x + 1
          if (x >= 0 && x < size)
          y <- mineCoordinates.y - 1 to mineCoordinates.y + 1
          if (y >= 0 && y < size)
          if (Coordinates(x, y) != mineCoordinates)
          // took only coordinates adjacent to the mine.
        do
          cells = cells.updateWhere((coordinates, _) => coordinates == Coordinates(x, y), (coordinates, cell) => cell match
            case Cell.EmptyCell(n) => (coordinates, Cell.EmptyCell(n+1))
            case Cell.MineCell() => (coordinates, Cell.MineCell()))
    )

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    var res = cells
      .find(_ == Coordinates(x, y) && _ != Cell.MineCell())
      .map((_, cell) => cell match
        case Cell.EmptyCell(n) => n
        case Cell.MineCell() => throw new IllegalStateException())
    OptionToOptional(res)

  def won = false

object LogicsImpl:
  def apply(size: Int, mines: Int): LogicsImpl =
    var random = Random();
    var mineCoordinates = Sequence[Coordinates]()
    while mineCoordinates.length() < mines do
      val coordinates = Coordinates(random.nextInt(size), random.nextInt(size))
      if !mineCoordinates.contains(coordinates) then
        mineCoordinates = mineCoordinates.concat(Sequence(coordinates))

    new LogicsImpl(size, mineCoordinates)

  // HELP PLEASE
  // Without that uselessParam i get this error (i think it is due to type inference of the
  // overloaded apply method. Introducing this parameter solved the error):
  //
  // /Users/Alessandro/Desktop/pps-23-24-lab05/src/main/java/polyglot/a01b/GUI.java
  // cannot access util.Sequences.Sequence
  //  bad class file: /Users/Alessandro/Desktop/pps-23-24-lab05/target/scala-3.3.1/classes/util/Sequences$Sequence.class
  //    undeclared type variable: B
  //    Please remove or make sure it appears in the correct subdirectory of the classpath.
  // LogicsImpl.apply
  def apply(size: Int, mineCoordinates: Sequence[Coordinates], uselessParam: Double = 0.0): LogicsImpl =
    new LogicsImpl(size, mineCoordinates)
