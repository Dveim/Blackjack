package cards

sealed trait Suit

object Hearts extends Suit
object Diamonds extends Suit
object Clubs extends Suit
object Spades extends Suit

sealed trait Value

object Ace extends Value
object Two extends Value
object Three extends Value
object Four extends Value
object Five extends Value
object Six extends Value
object Seven extends Value
object Eight extends Value
object Nine extends Value
object Ten extends Value
object Jack extends Value
object Queen extends Value
object King extends Value

object Cards {
  val suits = List(Hearts, Diamonds, Clubs, Spades)
  val values = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

  def deck: List[Card] = for (s <- suits;
                              v <- values) yield Card(s, v)
}

case class Card(s: Suit, v: Value) {
  override def toString = v + " of " + s //todo unicode
}
