package com.dveim.states

import com.dveim.cards.{Card, Shoe}

case class State(cash: Int,
                 bet: Int,
                 shoe: Shoe,
                 playerCards: List[Card],
                 dealerCards: List[Card]) {

  def this(s: State) = this(s.cash, s.bet, s.shoe, s.playerCards, s.dealerCards)

  def getInput: String = io.StdIn.readLine("Your move: ")
}

trait StartS extends State {
  def transit = new State(copy(bet = this.bet + 50)) with DealS
}

trait DealS extends State {
  def transit = new State(copy(cash = this.cash + 50)) with StartS
}
