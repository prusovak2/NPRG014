package h1
import scala.language.implicitConversions

// Add necessary class and object definitions in order to make the statements in the main work.

object I extends Complex(0, 1)

class Complex(val real: Int, val imaginary: Int) {
  override def toString(): String = {
    val imaginaryPart = imaginary match {
      case 0                    => ""
      case value if (value > 0) => s"+${value}"
      case value                => s"${value}"
    }
    return s"${real}${imaginaryPart}i"
  }

  def unary_- = new Complex(-real, -imaginary)
  def +(other: Complex) =
    Complex(real + other.real, imaginary + other.imaginary)
  def *(other: Complex) = Complex(
    real * other.real - imaginary * other.imaginary,
    real * other.imaginary + imaginary * other.real
  )
  def -(other: Complex) =
    Complex(real - other.real, imaginary - other.imaginary)

}

object Complex {
  given Conversion[Int, Complex] = i => new Complex(i, 0)
}

object ComplexNumbers:
  def main(args: Array[String]): Unit =
    println(Complex(1, 2)) // 1+2i
    println(1 + 2 * I + I * 3 + 2) // 3+5i

    val c = (2 + 3 * I + 1 + 4 * I) * I
    println(-c) // 7-3i
    //println(1 + 2 * I - I * 3 + 2)
