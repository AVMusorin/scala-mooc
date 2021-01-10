package homeworks.collections

import scala.annotation.tailrec

object task_seq_riddle {

  /**
   * Рассмотрим последовательность с числами:
   *
   * 1
   * 1 1
   * 2 1
   * 1 2 1 1
   * 1 1 1 2 2 1
   * 3 1 2 2 1 1
   * ...........
   *
   * 1. Реализуйте функцию генерирующую след последовательность из текущей
   * */

  def nextLine(currentLine: List[Int]): List[Int] = {
    @tailrec
    def recValuesAcc(element: Int, tail: List[Int], seq: List[Int], currentAcc: Int = 1): List[Int] = {
      tail match {
        case x :: Nil if x == element => seq ++ List(currentAcc + 1, element)
        case x :: Nil => seq ++ List(currentAcc, element) ++ List(1, x)
        case x :: xs if x == element => recValuesAcc(x, xs, seq, currentAcc + 1)
        case x :: xs => recValuesAcc(x, xs, seq ++ List(currentAcc, element))
        case Nil => seq
      }
    }
    currentLine match {
      case Nil => List(1)
      case h :: Nil => List(1, h)
      case h :: tail => recValuesAcc(h, tail, List.empty[Int])
    }
  }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  val funSeq: LazyList[List[Int]] = List(1) #:: funSeq.map(nextLine)
}