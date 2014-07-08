package states

import cards._
import Event.getEvent
import io.StdIn.readInt

case class State(cash: Int,
                 bet: Int,
                 shoe: Shoe,
                 playerCards: List[Card],
                 dealerCards: List[Card]) {

  def this(s: State) = this(s.cash, s.bet, s.shoe, s.playerCards, s.dealerCards)

  def getBet: Int = {
    println("Please, enter your bet")
    val _bet = readInt()
    println()
    require(1 <= _bet && _bet <= cash)
    _bet
  }

  def getScore(cards: List[Card]): Int = {
    val (aces, other) = cards.partition(_.v == Ace)
    val sumOfOther = other.map(_.v match {
      case Two => 2
      case Three => 3
      case Four => 4
      case Five => 5
      case Six => 6
      case Seven => 7
      case Eight => 8
      case Nine => 9
      case _ => 10 // aces can't be here
    }).sum
    aces.foldLeft(sumOfOther)((res, _) => if (res < 11) res + 11 else res + 1)
  }
}

sealed trait Transition extends State {
  def _transit: State with Transition

  def printCards(cards: List[Card], message: String) {
    println(message)
    cards foreach (x => print(s"$x  "))
    print(s"with total score ${getScore(cards)}\n")
  }

  def printInfo() {
    printCards(playerCards, "Player cards:")
    printCards(dealerCards, "Dealer cards:")
    println(s"\nYour cash - $cash, current bet - $bet")
  }

  def transit = {
    printInfo()
    shoe.checkForShuffle()
    _transit
  }
}

trait StartS extends Transition {
  //todo check for lose
  //todo why always ask for deal only?
  override def printCards(cards: List[Card], message: String) {}

  override def _transit = getEvent match {
    case Deal =>
      try {
        val _bet = getBet
        val playerCard1 = shoe.getCard
        val dealerCard = shoe.getCard
        val playerCard2 = shoe.getCard

        new State(
          cash - _bet,
          bet + _bet,
          shoe,
          playerCard1 :: playerCard2 :: playerCards,
          dealerCard :: dealerCards) with DealS

      } catch {
        case e@(_: NumberFormatException | _: IllegalArgumentException) =>
          println(s"Please, enter a valid 1 <= integer <= $cash:")
          this
      }

    case _ =>
      println("You can only press `deal` at this phase")
      this
  }
}

trait DealS extends State with Transition {
  override def _transit = getEvent match {
    case Hit =>
      val newPlayerCards = shoe.getCard :: playerCards
      val playerScore = getScore(newPlayerCards)

      if (playerScore > 21) {
        println("You busted")
        new State(cash, 0, shoe, Nil, Nil) with StartS

      } else
      // changed shoe too
        new State(copy(playerCards = newPlayerCards)) with DealS

    case Stand =>
      new State(copy()) with StoodS

    case _ =>
      println("You can only press `hit` or `stand` at this phase")
      this
  }
}

trait StoodS extends State with Transition {
  override def printInfo() {}

  override def _transit = {
    val dealerCard = shoe.getCard
    println(s"Dealer got card $dealerCard")
    val newDealerCards = dealerCard :: dealerCards
    val dealerScore = getScore(newDealerCards)

    // dealer hits again
    if (dealerScore < 17)
      new State(copy(dealerCards = newDealerCards)) with StoodS

    // dealer stands (not busted)
    else if (dealerScore < 22) {
      val playerScore = getScore(playerCards)

      if (playerScore > dealerScore) {
        println("You win")
        new State(cash + 2 * bet, 0, shoe, Nil, Nil) with StartS

      } else if (playerScore < dealerScore) {
        println("You lose")
        new State(cash, 0, shoe, Nil, Nil) with StartS

      } else {
        println("Nobody wins or loses")
        new State(cash + bet, 0, shoe, Nil, Nil) with StartS
      }
    }

    // dealer busts
    else {
      println("Dealer busts")
      new State(cash + 3 * bet / 2, 0, shoe, Nil, Nil) with StartS
    }
  }
}
