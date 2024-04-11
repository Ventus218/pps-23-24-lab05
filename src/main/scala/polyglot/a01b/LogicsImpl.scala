package polyglot.a01b

import polyglot.OptionToOptional
import util.Optionals.Optional as ScalaOptional
import polyglot.a01b.Logics
import util.Sequences.Sequence
import util.Sequences.Sequence.*

import scala.jdk.javaapi.OptionConverters

// ***** UTILITY EXTENSIONS *****
extension [E] (s: Sequence[E])
  def length() : Int = s match
    case Cons(h, t) => 1 + t.length()
    case _ => 0

/** solution and descriptions at https://bitbucket.org/mviroli/oop2019-esami/src/master/a01b/sol2/ */
class LogicsImpl(private val size: Int, private val mines: Int) extends Logics:

  private var mineCoordinates = Sequence[Coordinates]()

  def hit(x: Int, y: Int): java.util.Optional[Integer] =
    if mineCoordinates.contains(Coordinates(x, y)) then
      OptionToOptional(ScalaOptional.Empty())
    else
      OptionToOptional(ScalaOptional.Just(0))

  def won = false

object LogicsImpl:
  def apply(size: Int, mines: Int): LogicsImpl =
    new LogicsImpl(size, mines)

  def apply(size: Int, mineCoordinates: Sequence[Coordinates]): LogicsImpl =
    val logics = new LogicsImpl(size, mineCoordinates.length())
    logics.mineCoordinates = mineCoordinates
    logics

case class Coordinates(x: Int, y: Int)