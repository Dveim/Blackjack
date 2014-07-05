package com.dveim.states

import com.dveim.cards.{Card, Shoe}

sealed trait State {
  val cash: Int
  val bet: Int
  val shoe: Shoe
  val playerCards: List[Card]
  val dealerCards: List[Card]

  def transit(s: State): State

  def getInput: String = io.StdIn.readLine("Your move: ")
}

case class StartS(numberOfDecks: Int = 4, cash: Int = 100) extends State {
  override val bet: Int = 0
  override val playerCards: List[Card] = Nil
  override val dealerCards: List[Card] = Nil
  override val shoe: Shoe = new Shoe(numberOfDecks)

  override def transit(s: State): State = {
    ???
  }
}

