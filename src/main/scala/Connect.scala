import Direction.*

enum Color:
   case Black, White
enum Direction:
   case N, NE, NEE, E, S, SW, SWW, W

case class Pos(x: Int, y: Int)
extension (p: Pos)
   private def vicinity(d: Direction): Pos = d match
      case Direction.N   => Pos(p.x - 1, p.y)
      case Direction.NE  => Pos(p.x - 1, p.y + 1)
      case Direction.NEE => Pos(p.x - 1, p.y + 2)
      case Direction.E   => Pos(p.x, p.y + 1)
      case Direction.S   => Pos(p.x + 1, p.y)
      case Direction.SW  => Pos(p.x + 1, p.y - 1)
      case Direction.SWW => Pos(p.x + 1, p.y - 2)
      case Direction.W   => Pos(p.x, p.y - 1)

class Connect(board: List[String]):
   private val (maxX, maxY) = (board.head.indices.last, board.indices.last)
   // board.zipWithIndex.map((l, i) => " " * i + l).foreach(println)
   // println("-" * 20)
   // board.foreach(println)
   // println("-" * 20)
   private val startRowBlack: Int = board.map(_.head).indexOf('X')
   private val startColWhite: Int = board.head.indexOf('O')
   private val startPosBlack: Option[Pos] = if startRowBlack == -1 then None else Some(Pos(startRowBlack, 0))
   private val startPosWhite: Option[Pos] = if startColWhite == -1 then None else Some(Pos(0, startColWhite))

   // println(s"Black at $startPosBlack, White at $startPosWhite")

   private def charAtPos(p: Pos): Option[Char] = Option(board(p.x)(p.y))

   private def validNeighbours(c: Char, p: Pos): Seq[Pos] =
      (for d <- Direction.values yield p.vicinity(d))
         .filterNot(p => p.x < 0)
         .filterNot(p => p.x > maxX)
         .filterNot(p => p.y < 0)
         .filterNot(p => p.y > maxY)
         .filter(p => charAtPos(p).get == c)

   println(validNeighbours('X', startPosBlack.getOrElse(Pos(-1, -1))))

   def winner: Option[Color] =
      val whiteWin = board.size == 1 & startPosWhite.isDefined
      val blackWin = board.size == 1 & startPosBlack.isDefined
      (whiteWin, blackWin) match
         case (true, false) => Some(Color.White)
         case (false, true) => Some(Color.Black)
         case _             => None
