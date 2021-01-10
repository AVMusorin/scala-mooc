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
    /**
     * Returns reversed list
     */
    @tailrec
    def recValuesAcc(element: Int, tail: List[Int], seq: List[Int], currentAcc: Int = 1): List[Int] = {
      // added elements in the head of the list to avoid scanning full list in case of adding in tail
      tail match {
        case x :: Nil if x == element => element :: currentAcc + 1 :: seq
        case x :: Nil => x :: 1 :: element :: currentAcc :: seq
        case x :: xs if x == element => recValuesAcc(x, xs, seq, currentAcc + 1)
        case x :: xs => recValuesAcc(x, xs, element :: currentAcc :: seq)
        case Nil => seq
      }
    }
    currentLine match {
      case Nil => List(1)
      case h :: Nil => List(1, h)
      case h :: tail => recValuesAcc(h, tail, List.empty[Int]).reverse
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