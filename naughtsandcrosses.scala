import scala.collection.mutable.Map

case class Coord (x: Int, y: Int)


var board: Map[Coord, Option[Char]] = Map(Coord(0,0) -> None, Coord(0,1) -> None, Coord(0,2) -> None, Coord(1,0) -> None, Coord(1,1) -> None, Coord(1,2) -> None, Coord(2,0) -> None, Coord(2,1) -> None, Coord(2,2) -> None)
printBoard
printBoardWithNumbers
/*board.put(Coord(1,2), Some("X"))
printBoard

board.put(Coord(0,2), Some("O"))
printBoard

board.put(Coord(1,1), Some("X"))
printBoard

board.put(Coord(1,0), Some("O"))
printBoard */
var turn: Char = 'X'

while (!isGameOver) {
  printBoard
  println("Enter a number between 1 and 9 to start your go")
  var input = Console.readInt
  if (input < 1 || input > 9) {
    println("You idiot, try again")
  }
  var xAxis = (input - 1) / 3
  var yAxis = (input - 1) % 3

  board.put(Coord(xAxis, yAxis), Some(turn))
  printBoard
  println()
  if (turn == 'X') turn = 'O'
  else turn = 'X'
}

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

def printBoardWithNumbers = {
  println("**********")
  var i = 1  
  for {
      x <- 0 to 2
      y <- 0 to 2
    } {
    print(i)
    i += 1  
    if(y == 2) println()
    else print("|")
    
  }
}

def isGameOver = {
  false
}

def gameResult = {
  
}