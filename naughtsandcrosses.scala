import scala.collection.mutable.Map

case class Coord (x: Int, y: Int)


var board: Map[Coord, Option[Char]] = Map(Coord(0,0) -> None, Coord(0,1) -> None, Coord(0,2) -> None, Coord(1,0) -> None, Coord(1,1) -> None, Coord(1,2) -> None, Coord(2,0) -> None, Coord(2,1) -> None, Coord(2,2) -> None)
println("Here is the board. Each number represents a spot for an X or O")
printBoardWithNumbers

/*Start game with X*/
var turn: Char = 'O'


println("X will go first")
var validPlayCount = 0

while (!isGameOver) {
	
		if (turn == 'X') {
		  turn = 'O'
		}
		else {
		  turn = 'X'  
		} 

  println("%s's turn (go %d)). Enter a number between 1 and 9 to have your turn" format (turn, validPlayCount+1))
  var input = Console.readInt

  if (isWithinRange(input)) {
    println("Nope, a number between 1 and 9, try again")
  }

  var xAxis = (input - 1) / 3
  var yAxis = (input - 1) % 3

	if (!canPlayThisSquare(Coord(xAxis,yAxis))) {
		println("This square has been played. Try again")
  }
  else {
		board.put(Coord(xAxis, yAxis), Some(turn))
		printBoard
		println()
    validPlayCount += 1
 }
}


	if(haveIWon('_')) println("Well done! %s is the winner!!" format (turn))
	if(isDraw('_')) println("It's a draw!")



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
  haveIWon('X') || haveIWon('O') || isDraw('_')
}

def haveIWon(letter: Char): Boolean = {
   (		board.get(Coord(0,0)) == Some(Some(letter)) && board.get(Coord(0,1)) == Some(Some(letter)) && board.get(Coord(0,2)) == Some(Some(letter))
     || board.get(Coord(1,0)) == Some(Some(letter)) && board.get(Coord(1,1)) == Some(Some(letter)) && board.get(Coord(1,2)) == Some(Some(letter))
     || board.get(Coord(2,0)) == Some(Some(letter)) && board.get(Coord(2,1)) == Some(Some(letter)) && board.get(Coord(2,2)) == Some(Some(letter))
     || board.get(Coord(0,0)) == Some(Some(letter)) && board.get(Coord(1,0)) == Some(Some(letter)) && board.get(Coord(2,0)) == Some(Some(letter))
     || board.get(Coord(0,1)) == Some(Some(letter)) && board.get(Coord(1,1)) == Some(Some(letter)) && board.get(Coord(2,1)) == Some(Some(letter))
     || board.get(Coord(0,2)) == Some(Some(letter)) && board.get(Coord(1,2)) == Some(Some(letter)) && board.get(Coord(2,2)) == Some(Some(letter))
     || board.get(Coord(0,0)) == Some(Some(letter)) && board.get(Coord(1,1)) == Some(Some(letter)) && board.get(Coord(2,2)) == Some(Some(letter))
     || board.get(Coord(0,2)) == Some(Some(letter)) && board.get(Coord(1,1)) == Some(Some(letter)) && board.get(Coord(2,0)) == Some(Some(letter)))
}

def isDraw(letter: Char) :Boolean = {
		(validPlayCount == 9) && (haveIWon(letter) == false)
}

def isCellFull(input: Int): Boolean = {
  var xAxis = (input - 1) / 3
  var yAxis = (input - 1) % 3

	(board.get(Coord(xAxis,yAxis)) != None)
}




def canPlayThisSquare(coord: Coord): Boolean = {
	board.get(coord) match {
	  case Some(None) => {
			// Nothing in this square
			true
	  }
	  case _ => {
			// There is something already in this square
			false
	  }
	}
}


def gameResult = {

}

def isWithinRange(input: Int): Boolean = {
  (input < 1 || input > 9)
}

