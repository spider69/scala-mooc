package homeworks.collections

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

  def nextLine(currentLine: List[Int]): List[Int] =
    currentLine.foldRight(List[Int]()) { (curElem, acc) =>
      acc match {
        case count :: elem :: tail if elem == curElem => (count + 1) :: elem :: tail
        case elems => 1 :: curElem :: elems
      }
    }

  /**
   * 2. Реализуйте ленивый список, который генерирует данную последовательность
   * Hint: см. LazyList.cons
   *
   * lazy val funSeq: LazyList[List[Int]]  ...
   *
   */

  lazy val funSeq: LazyList[List[Int]] = {
    def lazyNextLine(currentLine: List[Int] = List(1)): LazyList[List[Int]] =
      currentLine #:: lazyNextLine(nextLine(currentLine))

    lazyNextLine()
  }
}