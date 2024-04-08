package ex

import scala.annotation.targetName

// Express a second degree polynomial
// Structure: secondDegree * X^2 + firstDegree * X + constant
trait SecondDegreePolynomial:
  def constant: Double
  def firstDegree: Double
  def secondDegree: Double
  def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial
  def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial

case class SecondDegreePolynomialImpl(val secondDegree: Double, val firstDegree: Double, val constant: Double) extends SecondDegreePolynomial {

  override def +(polynomial: SecondDegreePolynomial): SecondDegreePolynomial = SecondDegreePolynomialImpl(secondDegree + polynomial.secondDegree, firstDegree + polynomial.firstDegree, constant + polynomial.constant)

  override def -(polynomial: SecondDegreePolynomial): SecondDegreePolynomial = SecondDegreePolynomialImpl(secondDegree - polynomial.secondDegree, firstDegree - polynomial.firstDegree, constant - polynomial.constant)
}

// object SecondDegreePolynomial:
//   def apply(secondDegree: Double, firstDegree: Double, constant: Double): SecondDegreePolynomial = new SecondDegreePolynomialImpl(secondDegree, firstDegree, constant)

@main def checkComplex(): Unit =
  val simplePolynomial = SecondDegreePolynomialImpl(1.0, 0, 3)
  val anotherPolynomial = SecondDegreePolynomialImpl(0.0, 1, 0.0)
  val fullPolynomial = SecondDegreePolynomialImpl(3.0, 2.0, 5.0)
  val sum = simplePolynomial + anotherPolynomial
  println((sum, sum.secondDegree, sum.firstDegree, sum.constant)) // 1.0 * X^2 + 1.0 * X + 3.0
  val multipleOperations = fullPolynomial - (anotherPolynomial + simplePolynomial)
  println((multipleOperations, multipleOperations.secondDegree, multipleOperations.firstDegree, multipleOperations.constant)) // 2.0 * X^2 + 1.0 * X + 2.0
  println(SecondDegreePolynomialImpl(0, 0, 0) == SecondDegreePolynomialImpl(0, 0, 0))

/** Hints:
  *   - implement SecondDegreePolynomial with a SecondDegreePolynomialImpl class, similar to PersonImpl in slides
  *   - check that equality and toString do not work
  *   - use a case class SecondDegreePolynomialImpl instead
  *   - check equality and toString now
  */
