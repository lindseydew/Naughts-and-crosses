import scala.collection.mutable.Map

case class Coord (x: Int, y: Int)


var board: Map[Coord, Option[String]] = Map(Coord(0,0) -> None, Coord(0,1) -> None, Coord(0,2) -> None, Coord(1,0) -> None, Coord(1,1) -> None, Coord(1,2) -> None, Coord(2,0) -> None, Coord(2,1) -> None, Coord(2,2) -> None)
printBoard
board.put(Coord(1,2), Some("X"))
printBoard

board.put(Coord(0,2), Some("O"))
printBoard

board.put(Coord(1,1), Some("X"))
printBoard

board.put(Coord(1,0), Some("O"))
printBoard 


def printBoard = { 
	println("********")
	for {
		x <- 0 to 2
		y <- 0 to 2
	} {
		print(board(Coord(x,y)) getOrElse " ")
		if(y == 2) println()
		else print("|")
  }
}
