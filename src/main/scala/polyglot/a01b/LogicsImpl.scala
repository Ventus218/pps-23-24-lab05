package polyglot.a01b

import polyglot.OptionToOptional
import util.Sequences.Sequence
import util.Sequences.Sequence.*
import scala.util.Random
import ex.get
import util.Optionals.Optional

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
trait Coordinates:
  val x: Int
  val y: Int
object Coordinates {
  private case class CoordinatesImpl(x: Int, y: Int) extends Coordinates
  def apply(x: Int, y: Int): Coordinates = CoordinatesImpl(x, y)
}

trait Grid[E]:
  val size: Int
  def get(coordinates: Coordinates): E
  def change(coordinates: Coordinates, changeFunc: E => E): Unit
  def findFirst(predicate: E => Boolean): Optional[Coordinates]
object Grid:
  private class GridImpl[E](val size: Int) extends Grid[E]:
    private var cells: Sequence[(Coordinates, E)] = Sequence()
    def initCell(coordinates: Coordinates, e: E): Unit =
      if cells.find((c, _) => c == coordinates).isEmpty then
        cells = cells concat Sequence((coordinates, e))
      else
        throw new IllegalStateException("Grid cell already initialized")
    override def get(coordinates: Coordinates): E =
      cells.find((c, _) => c == coordinates).get()._2
    override def change(coordinates: Coordinates, changeFunc: E => E): Unit =
      cells = cells.updateWhere((c, _) => c == coordinates, (_, e) => (coordinates, changeFunc(e)))
    override def findFirst(predicate: E => Boolean): Optional[Coordinates] =
      cells.find((_, e) => predicate(e)).map(_._1)

  def apply[E](size: Int, gridFiller: Coordinates => E): Grid[E] =
    val gridImpl = new GridImpl[E](size)
    for
      x <- 0 until size
      y <- 0 until size
    do
      val coordinates = Coordinates(x, y)
      gridImpl.initCell(coordinates, gridFiller(coordinates))
    gridImpl


// ***** ACTUAL LOGICS IMPLEMENTATION *****
/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl (private val size: Int, private val mineCoordinates: Sequence[Coordinates]) extends Logics:

  private enum Cell:
    case MineCell()
    case EmptyCell(adjacentMines: Int, wasHit: Boolean)

  private val grid = Grid(size, coordinates => if mineCoordinates.contains(coordinates) then Cell.MineCell() else Cell.EmptyCell(0, false))

  computeAdjacentMineNumbers()
  private def computeAdjacentMineNumbers(): Unit =
    mineCoordinates.foreach(
      mineCoordinates =>
        for
          x <- mineCoordinates.x - 1 to mineCoordinates.x + 1
          if (x >= 0 && x < size)
          y <- mineCoordinates.y - 1 to mineCoordinates.y + 1
          if (y >= 0 && y < size)
          if (Coordinates(x, y) != mineCoordinates)
          // took only coordinates adjacent to the mine.
        do
          val coordinates = Coordinates(x, y)
          grid.change(coordinates, _ match
            case Cell.EmptyCell(n, h) => Cell.EmptyCell(n + 1, h)
            case Cell.MineCell() => Cell.MineCell())
    )

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    val coordinates = Coordinates(x, y)
    val res = grid.get(coordinates) match
      case Cell.MineCell() => Optional.Empty()
      case Cell.EmptyCell(n, _) => Optional.Just(n)

    if !res.isEmpty then
      grid.change(coordinates, cell => cell match
        case Cell.EmptyCell(n, false) => Cell.EmptyCell(n, true)
        case _ => cell)
    OptionToOptional(res)

  def won: Boolean =
    // If there is no cell which was not hit then the player must have won
    grid.findFirst(_ match
      case Cell.EmptyCell(_, false) => true
      case _ => false
      ).isEmpty

object LogicsImpl:
  def apply(size: Int, mines: Int): LogicsImpl =
    val random = Random()
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
