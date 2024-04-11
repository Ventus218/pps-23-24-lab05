package polyglot.a01b

import org.junit.jupiter.api.*
import org.junit.jupiter.api.Assertions.*
import util.Sequences.Sequence
import java.util.Optional
class LogicsImplTest:

  private val size = 4
  private val mines = 2
  private val mineCoordinates = Sequence(Coordinates(2, 0), Coordinates(2, 1))

  var logics: Logics = _

  //    0 1 2 3
  // 0 |0|2|X|2|
  // 1 |0|2|X|2|
  // 2 |0|1|1|1|
  // 3 |0|0|0|0|
  @BeforeEach def init(): Unit =
    logics = LogicsImpl(size, mineCoordinates)

  @Test def gameIsNotWonAtStart(): Unit =
    assertEquals(false, logics.won())

  @Test def minesArePlacedCorrectly() : Unit =
    var correctlyPlacedMines = 0
    var countedMines = 0
    for
      x <- 0 until size
      y <- 0 until size
    do
      if logics.hit(x, y).isEmpty then
        countedMines += 1
        if mineCoordinates.contains(Coordinates(x, y)) then
          correctlyPlacedMines += 1

    assertEquals(mines, countedMines)
    assertEquals(mines, correctlyPlacedMines)

  @Test def adjacentNumberOfMinesIsCorrectForEveryCell(): Unit =
    assertEquals(Optional.of(0), logics.hit(0, 0))
    assertEquals(Optional.of(0), logics.hit(0, 1))
    assertEquals(Optional.of(0), logics.hit(0, 2))
    assertEquals(Optional.of(0), logics.hit(0, 3))
    assertEquals(Optional.of(0), logics.hit(1, 3))
    assertEquals(Optional.of(0), logics.hit(2, 3))
    assertEquals(Optional.of(0), logics.hit(3, 3))

    assertEquals(Optional.of(2), logics.hit(1, 0))
    assertEquals(Optional.of(2), logics.hit(1, 1))
    assertEquals(Optional.of(2), logics.hit(3, 0))
    assertEquals(Optional.of(2), logics.hit(3, 1))

    assertEquals(Optional.of(1), logics.hit(1, 2))
    assertEquals(Optional.of(1), logics.hit(2, 2))
    assertEquals(Optional.of(1), logics.hit(3, 2))

  @Test def gameIsWonAfterAllNonMineCellsAreHit(): Unit =
    for
      x <- 0 until size
      y <- 0 until size
      if (!mineCoordinates.contains(Coordinates(x, y)))
    do
      logics.hit(x, y)

    assertTrue(logics.won())