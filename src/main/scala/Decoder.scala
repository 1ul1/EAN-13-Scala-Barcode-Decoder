import Types.{Bit, Digit, Even, Odd, NoParity, One, Parity, Pixel, Str, Zero}
import scala.collection.immutable

object Decoder {
  // TODO 1.1
  def toBit(s: Char): Bit = {
    if (s.toInt % 2 == 0) Zero else One
  }
  def toBit(s: Int): Bit = if (s % 2 == 0) Zero else One

  // TODO 1.2
  def complement(c: Bit): Bit = c match {
    case Zero => One
    case One => Zero
  }

  // TODO 1.3
  val LStrings: List[String] = List("0001101", "0011001", "0010011", "0111101", "0100011",
    "0110001", "0101111", "0111011", "0110111", "0001011")
  val leftOddList: List[List[Bit]] = LStrings.map(code => code.map(bit => toBit(bit)).toList) // codificări L
  val rightList: List[List[Bit]] = leftOddList.map(code => code.map(bit => complement(bit)).toList) // codificări R
//  def reverse(code: List[Bit]): List[Bit] = {
//    code.foldLeft(List[Bit]())((acc, bit) => bit :: acc)
//  }
  def reverseGeneric[A](l: List[A]): List[A] = {
    l.foldLeft(List[A]())((acc, elem) => elem :: acc)
  }
  val leftEvenList: List[List[Bit]] = rightList.map(code => reverseGeneric(code))// codificări  G
  
  // TODO 1.4
  def group[A](l: List[A]): List[List[A]] = {
    def aux(l: List[A], group: List[A], acc: List[List[A]]): List[List[A]] = l match {
      case Nil => group :: acc
      case head :: tail => {
        group match {
          case Nil => aux(tail, head :: group, acc)
          case x :: xs => {
            if (x != head) aux(l, Nil, group :: acc)
            else aux(tail, head :: group, acc)
          }
        }
      }
    }
    //aux(l, Nil, Nil).foldLeft(List[List[A]]())((acc, elem) => elem :: acc)
    reverseGeneric(aux(l, Nil, Nil))
  }

  // TODO 1.5
  def runLength[A](l: List[A]): List[(Int, A)] = l match{
    case Nil => Nil
    case _ :: _ => group(l).map(group => (group.length, group.head))
  }
  
  case class RatioInt(n: Int, d: Int) extends Ordered[RatioInt] {
    require(d != 0, "Denominator cannot be zero")
    private val gcd = BigInt(n).gcd(BigInt(d)).toInt
    val a = n / gcd // numărător
    val b = d / gcd // numitor

    override def toString: String = s"$a/$b"

    override def equals(obj: Any): Boolean = obj match {
      case that: RatioInt => this.a.abs == that.a.abs &&
        this.b.abs == that.b.abs &&
        this.a.sign * this.b.sign == that.a.sign * that.b.sign
      case _ => false
    }

    // TODO 2.1
    def -(other: RatioInt): RatioInt = RatioInt(a * other.b - other.a * b, b * other.b)
    def +(other: RatioInt): RatioInt = RatioInt(a * other.b + other.a * b, b * other.b)
    def *(other: RatioInt): RatioInt = RatioInt(a * other.a, b * other.b)
    def /(other: RatioInt): RatioInt = RatioInt(a * other.b, b * other.a)
    def negative(): Boolean = {
      if (a * b < 0) true else false
    }

    // TODO 2.2
    def compare(other: RatioInt): Int = {
      if (this == other) 0
      else {
        if (this.a.sign * this.b.sign != other.a.sign * other.b.sign) {
          if (this.a.sign * this.b.sign == 1) 1
          else -1
        } else {
          if (a.abs * other.b.abs > b.abs * other.a.abs) 1 else -1
        }
      }
    }
  }
  
  // TODO 3.1
  def scaleToOne[A](l: List[(Int, A)]): List[(RatioInt, A)] = {
    def aux(l: List[(Int, A)], acc: Int): Int = l match {
      case Nil => acc
      case (nr, _) :: tail => aux(tail, acc + nr)
    }
    l.map((nr, x) => (RatioInt(nr, aux(l, 0)), x))
  }

  // TODO 3.2
  def scaledRunLength(l: List[(Int, Bit)]): (Bit, List[RatioInt]) = {
    def aux[A](l: List[(Int, A)], acc: Int): Int = l match {
      case Nil => acc
      case (nr, _) :: tail => aux(tail, acc + nr)
    }
    l match {
      case Nil => (Zero, Nil)
      case (_, bit) :: _ => (bit, l.map((int, _) => (RatioInt(int, aux(l, 0)))))
    }
  }
  
  // TODO 3.3
  def toParities(s: Str): List[Parity] = {
    s.toList.map(character => if (character == 'L') Odd else Even)
  }
  
  // TODO 3.4
  val PStrings: List[String] = List("LLLLLL", "LLGLGG", "LLGGLG", "LLGGGL", "LGLLGG",
    "LGGLLG", "LGGGLL", "LGLGLG", "LGLGGL", "LGGLGL")
  val leftParityList: List[List[Parity]] = PStrings.map(code => toParities(code.toList))

  // TODO 3.5
  type SRL = (Bit, List[RatioInt])
  val leftOddSRL:  List[SRL] = {
    leftOddList.map(listBit => runLength(listBit)).map(listLength => scaledRunLength(listLength))
  }
  val leftEvenSRL:  List[SRL] = {
    leftEvenList.map(listBit => runLength(listBit)).map(listLength => scaledRunLength(listLength))
  }
  val rightSRL:  List[SRL] = {
    rightList.map(listBit => runLength(listBit)).map(listLength => scaledRunLength(listLength))
  }

  // TODO 4.1
  def distance(l1: SRL, l2: SRL): RatioInt = (l1, l2) match{
      case ((bit1, list1), (bit2, list2)) => {
        if (bit1 == bit2) list1.zip(list2).foldLeft(RatioInt(0, 1))((acc, pair)
        => if (pair._1.-(pair._2).negative()) acc.-(pair._1.-(pair._2)) else acc.+(pair._1.-(pair._2)))
        else RatioInt(100, 1)
      }
  }

  // TODO 4.2
  def bestMatch(SRL_Codes: List[SRL], digitCode: SRL): (RatioInt, Digit) = {
    SRL_Codes.map(codes => distance(codes, digitCode)).zipWithIndex.min
  }
  
  // TODO 4.3
  def bestLeft(digitCode: SRL): (Parity, Digit) = {
    (bestMatch(leftOddSRL, digitCode), bestMatch(leftEvenSRL, digitCode)) match {
      case ((ratio1, digit1), (ratio2, digit2)) => {
        if (ratio1.compare(ratio2) == -1) (Odd, digit1)
        else (Even, digit2)
      }
    }
  }
  
  // TODO 4.4
  def bestRight(digitCode: SRL): (Parity, Digit) = {
    (NoParity, bestMatch(rightSRL, digitCode)._2)
  }

  def chunkWith[A](f: List[A] => (List[A], List[A]))(l: List[A]): List[List[A]] = {
    l match {
      case Nil => Nil
      case _ =>
        val (h, t) = f(l)
        h :: chunkWith(f)(t)
    }
  }
  
  def chunksOf[A](n: Int)(l: List[A]): List[List[A]] =
    chunkWith((l: List[A]) => l.splitAt(n))(l)

  // TODO 4.5
  def findLast12Digits(rle:  List[(Int, Bit)]): List[(Parity, Digit)] = {
//    def size(acc: Int, rle:  List[(Int, Bit)]): Int = rle match {
//      case Nil => acc
//      case (int, bit) :: tail => size(acc + int, tail)
//    }
//    if (size(0, rle) != 95) Nil
    if(rle.length != 59) Nil
    def left(acc: Int, list: List[SRL]): List[SRL] = {
      if (acc == 6) list
      else left(acc + 1, scaledRunLength(rle.slice(3 + acc * 4, 3 + acc * 4 + 4)) :: list)
    }
    def right(acc: Int, list: List[SRL]): List[SRL] = {
      if (acc == 6) list
      else right(acc + 1, scaledRunLength(rle.slice(32 + acc * 4, 32 + acc * 4 + 4)) :: list)
    }
    reverseGeneric(left(0, List[SRL]())).map(list => bestLeft(list)) ++ reverseGeneric(right(0, List[SRL]())).map(list => bestRight(list))
  }

  // TODO 4.6
  def firstDigit(l: List[(Parity, Digit)]): Option[Digit] = {
    def aux(acc:Int, l: List[Parity]): Int = {
      if (acc == 9 || l == leftParityList(acc)) acc
      else aux(acc + 1, l)
    }
    Some(aux(0, l.slice(0, 6).map((parity, _) => parity)))
  }

  // TODO 4.7
  def checkDigit(l: List[Digit]): Digit = {
    def aux(acc: Int): Int = {
      if (acc == 11) 3 * l(11)
      else {
        if (acc % 2 == 0) 1 * l(acc) + aux(acc + 1)
        else 3 * l(acc) + aux(acc + 1)
      }
    }
    (10 - aux(0) % 10) % 10
  }
  
  // TODO 4.8
  def verifyCode(code: List[(Parity, Digit)]): Option[String] = {
    if (code.length != 13) Some("Error1")
    else if (code(0)._2 == firstDigit(code.drop(1)).getOrElse(0) || 
              code(12)._2 == checkDigit(code.drop(1).map((parity, digit) => digit))) {
      Some(code.map((parity, digit) => digit.toString).mkString)
    }
    else Some("Error2")
  }

  // TODO 4.9
  def solve(rle:  List[(Int, Bit)]): Option[String] = {
    verifyCode(((NoParity, firstDigit(findLast12Digits(rle)).getOrElse(0)) :: List[(Parity, Digit)]())  ++ findLast12Digits(rle))
  }
  
  def checkRow(row: List[Pixel]): List[List[(Int, Bit)]] = {
    val rle = runLength(row);

    def condition(sl: List[(Int, Pixel)]): Boolean = {
      if (sl.isEmpty) false
      else if (sl.size < 59) false
      else sl.head._2 == 1 &&
        sl.head._1 == sl.drop(2).head._1 &&
        sl.drop(56).head._1 == sl.drop(58).head._1
    }

    rle.sliding(59, 1)
      .filter(condition)
      .toList
      .map(_.map(pair => (pair._1, toBit(pair._2))))
  }
}


