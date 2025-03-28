import Direction.*
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

enum Color:
   case Black, White
enum Direction:
   case N, NE, NEE, E, S, SW, SWW, W

case class Pos(row: Int, col: Int)
extension (p: Pos)
   private def vicinity(d: Direction): Pos = d match
      case Direction.N   => Pos(p.row - 1, p.col)
      case Direction.NE  => Pos(p.row - 1, p.col + 1)
      case Direction.NEE => Pos(p.row - 1, p.col + 2)
      case Direction.E   => Pos(p.row, p.col + 1)
      case Direction.S   => Pos(p.row + 1, p.col)
      case Direction.SW  => Pos(p.row + 1, p.col - 1)
      case Direction.SWW => Pos(p.row + 1, p.col - 2)
      case Direction.W   => Pos(p.row, p.col - 1)

class Connect(board: List[String]):
   private val (maxRowIdx, maxColIdx) = (board.indices.last, board.head.indices.last)
   board.zipWithIndex.map((l, i) => " " * i + l).foreach(println)
   println("-" * 20)
   board.foreach(println)
   println("-" * 20)

   private val startRowBlack: List[Int] = findIndices(board.map(_.head), _ == 'X')
   private val startColWhite: List[Int] = findIndices(board.head.toList, _ == 'O')
   private val startPosBlack: List[Pos] = if startRowBlack.isEmpty then Nil else startRowBlack.map(Pos(_, 0))
   private val startPosWhite: List[Pos] = if startColWhite.isEmpty then Nil else startColWhite.map(Pos(0, _))

   println(s"Black at $startPosBlack, White at $startPosWhite")
   println(s"max RowIndex: $maxRowIdx, max ColIndex: $maxColIdx")

   private def findIndices[T](list: List[T], predicate: T => Boolean): List[Int] =
      list.zipWithIndex.collect {
         case (element, index) if predicate(element) => index
      }

   private def charAtPos(p: Pos): Char = board(p.row)(p.col)

   // tailrec
   private def checkPath(c: Char, ps: List[Pos], visited: List[Pos]): Boolean =
      val nextPos = (for d <- Direction.values yield ps.head.vicinity(d))
         .filterNot(_.row < 0)
         .filterNot(_.col < 0)
         .filterNot(_.row > maxRowIdx)
         .filterNot(_.col > maxColIdx)
         .filter(p => charAtPos(p) == c)

      if nextPos.isEmpty then false
      else
         println(nextPos.toSeq)
         false
      // checkPath(c, nextPos.toList, ps.head :: visited)

   def winner: Option[Color] =
      val midBordEmpty = board.nonEmpty && board.tail.nonEmpty && board.tail.init.forall(_.tail.init.forall(_ == '.'))
      val singularBoard = board.size == 1
      val checkWhite =
         if startPosWhite.isEmpty || midBordEmpty then false
         else if singularBoard && startPosWhite.nonEmpty then true
         else checkPath('O', startPosWhite, Nil)
      val checkBlack =
         if startPosBlack.isEmpty || midBordEmpty then false
         else if singularBoard && startPosBlack.nonEmpty then true
         else checkPath('X', startPosBlack, Nil)

      (checkBlack, checkWhite) match
         case (false, true) => Some(Color.White)
         case (true, false) => Some(Color.Black)
         case _             => None
