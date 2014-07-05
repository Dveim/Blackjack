package com.dveim.cards

//todo cleanup (case??? newlines)
sealed trait Suit

case object Hearts extends Suit

case object Diamonds extends Suit

case object Clubs extends Suit

case object Spades extends Suit

sealed trait Value

case object Ace extends Value

case object Two extends Value

case object Three extends Value

case object Four extends Value

case object Five extends Value

case object Six extends Value

case object Seven extends Value

case object Eight extends Value

case object Nine extends Value

case object Ten extends Value

case object Jack extends Value

case object Queen extends Value

case object King extends Value

object Cards {
  val suits = List(Hearts, Diamonds, Clubs, Spades)
  val values = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

  def deck: List[Card] = for (s <- suits;
                              v <- values) yield Card(s, v)
}

case class Card(s: Suit, v: Value) {
  override def toString = v + " of " + s //todo unicode
}
