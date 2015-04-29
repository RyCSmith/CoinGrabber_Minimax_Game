package coinGrabber
import scala.collection.mutable.Buffer

/**
 * Driver program for Coin Grabber game.
 * @author Ryan Smith
 * @version April 2015
 */
object CoinGrabber {
  //global vars for program - p1 = player, p2 = computer
  val board = Array(Array(-1, 15, 10, 5, 25), Array(5, 10, 25, 10, 5), 
        Array(10, 25, 1, 25, 10), Array(5, 10, 25, 10, 5), Array(25, 5, 10, 5, -2))
  var moveCounter = 0    //counter used to alternate turns between player and computer
  var continue = true    //controls loop continuing gameplay
  var grabber = 0        //indicates which player grabbed the 1 coin
  val p1loc = Array(0,0) //location of player
  val p2loc = Array(4,4) //location of computer
  var p1money = 0        //player's total money
  var p2money = 0        //computer's total money
  
/**
 * Main function. Loops, alternating between user and computer. Prompts user for move and makes it, allows computer to make moves.  
 */
  def main(args: Array[String]): Unit = {
    
    println("Welcome to the Game! \nWould you like to begin or prefer the computer? Enter \"P\" or \"C\"")
    val response = readChar()
    if (response == 'C')
      moveCounter = moveCounter + 1
    displayBoard()
    
    //loop here until a player takes the middle coin
    while (continue){
      if (moveCounter % 2 == 0){
        println("Player1's turn, please make a move; U, D, R, L")
        var badResponse = true;
        
        //loop here until the user provides a valid response
        while(badResponse){
          val response = readChar()
          if (verifyMove(response, 1)){
            makeMove(response, 1)
            displayBoard()
            badResponse = false
            moveCounter = moveCounter + 1
            println("You moved to: " + p1loc(0) + ", " + p1loc(1))
            println("Your money: $" + p1money + ". Computer's money: $" + p2money + ".")
          }
          else
            println("Invalid choice. Please enter U, D, L, R.") 
        }    
      }
      else {
        println("Computer's turn.")
        makeMove(chooseMove(), 2)
        displayBoard()
        moveCounter = moveCounter + 1
        println("Computer moved to: " + p2loc(0) + ", " + p2loc(1))
        println("Your money: $" + p1money + ". Computer's money: $" + p2money + ".")
      }
    }
    calculateResults()
  }  
  
/**
 * Checks to see if the desired move is possible from the current position. Returns true if so, false otherwise.  
 * @param response - Char representing desired move direction. player - Int indicating if the move should be checked for user or computer      
 */
  def verifyMove(response: Char, player: Int): Boolean = {  
    if (player == 1){
      if (response == 'U'){
        if(p1loc(0) > 0)
          if(board(p1loc(0) - 1)(p1loc(1)) != -2)
            return true
      }
      else if (response == 'D'){
        if(p1loc(0) < 4)
          if(board(p1loc(0) + 1)(p1loc(1)) != -2)
            return true
      }
      else if (response == 'L'){
        if(p1loc(1) > 0)
          if(board(p1loc(0))(p1loc(1) - 1) != -2)
            return true
      }
      else if (response == 'R'){
        if(p1loc(1) < 4)
          if(board(p1loc(0))(p1loc(1) + 1) != -2)
            return true
      }
    }
    else if (player == 2){
      if (response == 'U'){
        if(p2loc(0) > 0)
          if(board(p2loc(0) - 1)(p2loc(1)) != -1)
            return true
      }
      else if (response == 'D'){
        if(p2loc(0) < 4)
          if(board(p2loc(0) + 1)(p2loc(1)) != -1)
            return true
      }
      else if (response == 'L'){
        if(p2loc(1) > 0)
          if(board(p2loc(0))(p2loc(1) - 1) != -1)
            return true
      }
      else if (response == 'R'){
        if(p2loc(1) < 4)
          if(board(p2loc(0))(p2loc(1) + 1) != -1)
            return true
      }
    }
    return false
  }
  
/**
 * Moves a player. Updates the board (empty spaces represented with 0), player location and money.   
 * @param response - Char representing desired move direction. player - Int indicating if the move should be checked for user or computer      
 */
  def makeMove(response: Char, player: Int) {
    if (player == 1){
      board(p1loc(0))(p1loc(1)) = 0
      if (response == 'U'){
        p1loc(0) = p1loc(0) - 1
      }
      else if (response == 'D'){
        p1loc(0) = p1loc(0) + 1
      }
      else if (response == 'L'){
        p1loc(1) = p1loc(1) - 1
      }
      else if (response == 'R'){
        p1loc(1) = p1loc(1) + 1
      }
      p1money = p1money + board(p1loc(0))(p1loc(1))
      board(p1loc(0))(p1loc(1)) = -1
    }
    else if (player == 2){
      board(p2loc(0))(p2loc(1)) = 0
      if (response == 'U'){
        p2loc(0) = p2loc(0) - 1
      }
      else if (response == 'D'){
        p2loc(0) = p2loc(0) + 1
      }
      else if (response == 'L'){
        p2loc(1) = p2loc(1) - 1
      }
      else if (response == 'R'){
        p2loc(1) = p2loc(1) + 1
      }
      p2money = p2money + board(p2loc(0))(p2loc(1))
      board(p2loc(0))(p2loc(1)) = -2
    }
    //checks to see if the player or the computer have taken the center coin and ends game if so
    if ((p1loc(0) == 2 && p1loc(1) == 2)){
      continue = false;
      grabber = 1
    }
    else if (p2loc(0) == 2 && p2loc(1) == 2){
      continue = false;
      grabber = 2
    }
  }

/**
 * Displays a representation of the current board on the console.
 * # : the value of coin in that space
 * 0 : empty space
 * -1 : player 1
 * -2 : computer
 */
  def displayBoard() {
    for (x <- board){
      for (y <- x){
        if (y == 0)
          print("     ")
        else if (y == -1)
          print("   p1")
        else if (y == -2)
          print("   Co")
        else
          print("%5d".format(y))}
      println("")
    }
  }
  
/**
 * Processes the results of a finished game and displays.
 */
  def calculateResults() {
    if (grabber == 1 && p1money < 100)
      println("You grabbed the middle coin and forfeited the game!")
    else if (grabber == 2 && p2money < 100) //shouldn't happen
      println("Computer grabbed the middle coin and forfeited the game!")
    else {
      if (grabber == 1)
        print("You grabbed the coin. ")
      else
        print("Computer grabbed the coin. ")
      print("Your total: $" + p1money + ". Computer total: $" + p2money + ". ")
      if (p1money > p2money)
        println("You win!")
      else
        println("Computer wins!")
    }
  }
  
  
  /**
   * Plays game situations from each possible move available to computer from current location. Receives 
   * expected maximized minimum values from each move and returns Char ('U', 'D', 'L', 'R') representing
   * move with greatest expected value.
   */
  def chooseMove(): Char ={
    //create deep copy of game elements and package into a State
    val boardCopy = board.map(_.clone)
    var p1loc2 = new Array[Int](p1loc.length)
    var x = 0
    for (x <- 0 to p1loc.length - 1)
      p1loc2(x) = p1loc(x)
    var p2loc2 = new Array[Int](p2loc.length)
    var z = 0
    for (z <- 0 to p2loc.length - 1)
      p2loc2(z) = p2loc(z)
    val playerMoney = p1money
    val compMoney = p2money
    val currentState = State(boardCopy, p1loc2, p2loc2, playerMoney, compMoney)
    
    //hold variables indicating best move and value. bestValue starts at unattainably low value
    var bestValue = -100000
    var bestMove = 'N' //this stands for "no move" (this should never happen)
      
    //see what the best expected profit of each available option is
    if (verifyMove('U', 2)){
      val upState = movePlayerCreateState(currentState, 2, 'U')
      val upExpectation = evaluateMove(upState, 13, true)
      if (upExpectation > bestValue){
         bestMove = 'U'
         bestValue = upExpectation
      }
    }
    if (verifyMove('D', 2)){
      val downState = movePlayerCreateState(currentState, 2, 'D')
      val downExpectation = evaluateMove(downState, 13, true)
      if (downExpectation > bestValue){
         bestMove = 'D'
         bestValue = downExpectation
      }
    }
    if (verifyMove('L', 2)){
      val leftState = movePlayerCreateState(currentState, 2, 'L')
      val leftExpectation = evaluateMove(leftState, 13, true)
      if (leftExpectation > bestValue){
         bestMove = 'L'
         bestValue = leftExpectation
      }
    }
    if (verifyMove('R', 2)){
      val rightState = movePlayerCreateState(currentState, 2, 'R')
      val rightExpectation = evaluateMove(rightState, 13, true)
      if (rightExpectation > bestValue){
         bestMove = 'R'
         bestValue = rightExpectation
      }
    }
    bestMove   
  }
  
/**
 * Heuristic to evaluate the expected value of a move. This is defined in terms of benefit to the computer.
 * There are 9 cases:
 * 1 - computer is on the 1 coin and has more than 100, return 1000000 (comp win)
 * 2 - player is on the 1 coin and has more than 100, return -1000000 (player win)
 * 3 - computer is 1 move from center with more than 100 and player is more than 1 move from center, return 10000 (significant advantage)
 * 4 - player is 1 move from center with more than 100 and computer is more than 1 move from center, return -10000 (significant disadvantage)
 * 5 - computer is on the 1 coin with less than 100, return -1000000 (player win)
 * 6 - player is on the 1 coin with less than 100, return 1000000 (computer win)
 * 7 - player and computer are both 1 move away and computer has more money than player, return 10000 (significant advantage)
 * 8 - player and computer are both 1 move away and computer has less money than player, return -10000 (significant disadvantage)
 * 9 - default: neither player on 1 coin, neither in position from case 3 or 4, return (computer money - player money)  
 */
  def heuristic(state: State): Int ={
    if (((state.cLoc(0) == 2) && (state.cLoc(1) == 2)) && (state.c$ > 100) && (state.c$ > state.p$))
      return 1000000
    else if (((state.pLoc(0) == 2) && (state.pLoc(1) == 2)) && (state.p$ > 100) && (state.p$ > state.c$))
      return -1000000
    else if ((((state.cLoc(0) == 1 || state.cLoc(0) == 3) && state.cLoc(1) == 2) || ((state.cLoc(1) == 1 || state.cLoc(1) == 3) && state.cLoc(0) == 2)) && (state.c$ > 100) && (state.pLoc(0) != 2 || state.pLoc(1) != 2)) 
      return 10000
    else if ((((state.pLoc(0) == 1 || state.pLoc(0) == 3) && state.pLoc(1) == 2) || ((state.pLoc(1) == 1 || state.pLoc(1) == 3) && state.pLoc(0) == 2)) && (state.p$ > 100) && (state.cLoc(0) != 2 || state.cLoc(1) != 2)) 
      return -10000
    else if (((state.cLoc(0) == 2) && (state.cLoc(1) == 2)) && (state.c$ < 100))
      return -1000000
    else if (((state.pLoc(0) == 2) && (state.pLoc(1) == 2)) && (state.p$ < 100))
      return 1000000
    else if ((((state.pLoc(0) == 1 || state.pLoc(0) == 3) && state.pLoc(1) == 2) || ((state.pLoc(1) == 1 || state.pLoc(1) == 3) && state.pLoc(0) == 2)) && (((state.cLoc(0) == 1 || state.cLoc(0) == 3) && state.cLoc(1) == 2) || ((state.cLoc(1) == 1 || state.cLoc(1) == 3) && state.cLoc(0) == 2)) && (state.c$ - state.p$ > 0))
      return 10000
    else if ((((state.pLoc(0) == 1 || state.pLoc(0) == 3) && state.pLoc(1) == 2) || ((state.pLoc(1) == 1 || state.pLoc(1) == 3) && state.pLoc(0) == 2)) && (((state.cLoc(0) == 1 || state.cLoc(0) == 3) && state.cLoc(1) == 2) || ((state.cLoc(1) == 1 || state.cLoc(1) == 3) && state.cLoc(0) == 2)) && (state.c$ - state.p$ < 0))
      return -10000
    else
      return state.c$ - state.p$
  }

/**
 * Creates a deep copy of the State. Moves the player or computer to the given position. Updates values and returns the new State.
 * Leaves argument State unchanged.
 */
  def movePlayerCreateState(state: State, player: Int, move: Char): State ={
    //make a deep copy of the State
    val boardCopy = state.board.map(_.clone)
    var pLoc2 = new Array[Int](state.pLoc.length)
    var x = 0
    for (x <- 0 to state.pLoc.length - 1)
      pLoc2(x) = state.pLoc(x)    
    var cLoc2 = new Array[Int](state.cLoc.length)
    var z = 0
    for (z <- 0 to state.cLoc.length - 1)
      cLoc2(z) = state.cLoc(z)
    val p$ = state.p$
    val c$ = state.c$
    val newState = State(boardCopy, pLoc2, cLoc2, p$, c$)
    
    //make the desired move
    if (player == 1){
      newState.board(newState.pLoc(0))(newState.pLoc(1)) = 0
      if (move == 'U'){
        newState.pLoc(0) = newState.pLoc(0) - 1
      }
      else if (move == 'D'){
        newState.pLoc(0) = newState.pLoc(0) + 1
      }
      else if (move == 'L'){
        newState.pLoc(1) = newState.pLoc(1) - 1
      }
      else if (move == 'R'){
        newState.pLoc(1) = newState.pLoc(1) + 1
      }
      newState.p$ = newState.p$ + newState.board(newState.pLoc(0))(newState.pLoc(1))
      newState.board(newState.pLoc(0))(newState.pLoc(1)) = -1
    }
    else if (player == 2){
      newState.board(newState.cLoc(0))(newState.cLoc(1)) = 0
      if (move == 'U'){
        newState.cLoc(0) = newState.cLoc(0) - 1
      }
      else if (move == 'D'){
        newState.cLoc(0) = newState.cLoc(0) + 1
      }
      else if (move == 'L'){
        newState.cLoc(1) = newState.cLoc(1) - 1
      }
      else if (move == 'R'){
        newState.cLoc(1) = newState.cLoc(1) + 1
      }
      newState.c$ = newState.c$ + newState.board(newState.cLoc(0))(newState.cLoc(1))
      newState.board(newState.cLoc(0))(newState.cLoc(1)) = -1
    }
    newState   
  }
  
/**
 * Creates a list of all possible moves for the given player from the current position in this State.
 * Represents moves with Chars: 'U', 'D', 'L', 'R' and returns all possible as a List.
 */
  def getPossibleMoves(state: State, player: Int): List[Char] ={
    var list = List[Char]() 
    if (player == 1){
      if (state.pLoc(0) > 0){
        if (((state.pLoc(0) - 1) != state.cLoc(0)) || (state.pLoc(1) != state.cLoc(1)))
          list = 'U' :: list
      }
      if (state.pLoc(0) < 4){
        if (((state.pLoc(0) + 1) != state.cLoc(0)) || (state.pLoc(1) != state.cLoc(1)))
          list = 'D' :: list
      }
      if (state.pLoc(1) > 0){
        if ((state.pLoc(0) != state.cLoc(0)) || ((state.pLoc(1) - 1) != state.cLoc(1)))
          list = 'L' :: list
      }
      if (state.pLoc(1) < 4){
        if ((state.pLoc(0) != state.cLoc(0)) || ((state.pLoc(1) + 1) != state.cLoc(1)))
          list = 'R' :: list
      }
      return list
    }
    else {
      if (state.cLoc(0) > 0){
        if (((state.cLoc(0) - 1) != state.pLoc(0)) || (state.cLoc(1) != state.pLoc(1)))
          list = 'U' :: list
      }
      if (state.cLoc(0) < 4){
        if (((state.cLoc(0) + 1) != state.pLoc(0)) || (state.cLoc(1) != state.pLoc(1)))
          list = 'D' :: list
      }
      if (state.cLoc(1) > 0){
        if ((state.cLoc(0) != state.pLoc(0)) || ((state.cLoc(1) - 1) != state.pLoc(1)))
          list = 'L' :: list
      }
      if (state.cLoc(1) < 4){
        if ((state.cLoc(0) != state.pLoc(0)) || ((state.cLoc(1) + 1) != state.pLoc(1)))
          list = 'R' :: list
      }
      return list
    }
  }

/**
 * Implements minimax for this game. Alternates between the computer's move and player's move using the Boolean 'comp'.
 * Recurses until hitting leaf (center with 1 coin) or depth == 0 then calls heuristic which returns value in terms of
 * how favorable the State is to the computer's expected value. The computer chooses the maximum value and returns it. The
 * player chooses the minimum value and returns it.
 */
  def evaluateMove(state: State, depth: Int, comp: Boolean): Int = {
    //base cases: someone has taken 1 coin or depth has reached 0
    if ((state.pLoc(0) == 2 && state.pLoc(1) == 2) || (state.cLoc(0) == 2 && state.cLoc(1) == 2) || depth == 0)
      return heuristic(state)   
    //computer's move
    if (comp){
      var maxValue = -100000
      val list = getPossibleMoves(state, 2)
      for (x <- list){
        val newState = movePlayerCreateState(state, 2, x)
        val stateValue = evaluateMove(newState, depth - 1, false)
        if (stateValue > maxValue)
          maxValue = stateValue
      }
      return maxValue
    }
    //player's move
    else {
      var minValue = 100000
      val list = getPossibleMoves(state, 1)
      for (x <- list){
        val newState = movePlayerCreateState(state, 1, x)
        val stateValue = evaluateMove(newState, depth - 1, true)
        if (stateValue < minValue)
          minValue = stateValue
      }
      return minValue
    }
  }  
}