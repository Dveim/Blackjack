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

  def getBet = {
    println("Please, enter you bet :")
    val _bet = readInt()
    require(1 <= _bet && _bet <= cash)
    _bet
  }
}

sealed trait Transition {
  def transit: State with Transition
}

trait StartS extends State with Transition {
  override def transit = getEvent match {
    case Deal =>
      try {
        val _bet = getBet
        val playerCard1 = shoe.getCard
        val dealerCard = shoe.getCard
        val playerCard2 = shoe.getCard

        new State(
          bet + _bet,
          cash - _bet,
          shoe,
          playerCard1 :: playerCard2 :: playerCards,
          dealerCard :: dealerCards) with DealS

      } catch {
        case e@(_: NumberFormatException | _: IllegalArgumentException) =>
          println(s"Please, enter a valid 1 <= number <= $cash :")
          this
      }

    case _ =>
      println("You can only press `deal` at this phase")
      this
  }
}

trait DealS extends State with Transition {
  override def transit = getEvent match {
    case Start =>
      new State(copy(cash = cash + 50)) with StartS

    case NotRecognized =>
      println("Invalid input")
      this
  }
}
