package cards

sealed trait Suit

object Spades extends Suit
object Hearts extends Suit
object Diamonds extends Suit
object Clubs extends Suit

sealed trait Rank

object Ace extends Rank
object Two extends Rank
object Three extends Rank
object Four extends Rank
object Five extends Rank
object Six extends Rank
object Seven extends Rank
object Eight extends Rank
object Nine extends Rank
object Ten extends Rank
object Jack extends Rank
object Queen extends Rank
object King extends Rank

object Cards {
    val suits = List(Hearts, Diamonds, Clubs, Spades)
    val ranks = List(Ace, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten, Jack, Queen, King)

    def deck: List[Card] = for (s <- suits;
                                r <- ranks) yield Card(s, r)
}

case class Card(suit: Suit, rank: Rank) {
    override def toString = (suit, rank) match {
        case (Spades, Ace) => "\uD83C\uDCA1"
        case (Spades, Two) => "\uD83C\uDCA2"
        case (Spades, Three) => "\uD83C\uDCA3"
        case (Spades, Four) => "\uD83C\uDCA4"
        case (Spades, Five) => "\uD83C\uDCA5"
        case (Spades, Six) => "\uD83C\uDCA6"
        case (Spades, Seven) => "\uD83C\uDCA7"
        case (Spades, Eight) => "\uD83C\uDCA8"
        case (Spades, Nine) => "\uD83C\uDCA9"
        case (Spades, Ten) => "\uD83C\uDCAA"
        case (Spades, Jack) => "\uD83C\uDCAB"
        case (Spades, Queen) => "\uD83C\uDCAD"
        case (Spades, King) => "\uD83C\uDCAE"

        case (Hearts, Ace) => "\uD83C\uDCB1"
        case (Hearts, Two) => "\uD83C\uDCB2"
        case (Hearts, Three) => "\uD83C\uDCB3"
        case (Hearts, Four) => "\uD83C\uDCB4"
        case (Hearts, Five) => "\uD83C\uDCB5"
        case (Hearts, Six) => "\uD83C\uDCB6"
        case (Hearts, Seven) => "\uD83C\uDCB7"
        case (Hearts, Eight) => "\uD83C\uDCB8"
        case (Hearts, Nine) => "\uD83C\uDCB9"
        case (Hearts, Ten) => "\uD83C\uDCBA"
        case (Hearts, Jack) => "\uD83C\uDCBB"
        case (Hearts, Queen) => "\uD83C\uDCBD"
        case (Hearts, King) => "\uD83C\uDCBE"

        case (Diamonds, Ace) => "\uD83C\uDCC1"
        case (Diamonds, Two) => "\uD83C\uDCC2"
        case (Diamonds, Three) => "\uD83C\uDCC3"
        case (Diamonds, Four) => "\uD83C\uDCC4"
        case (Diamonds, Five) => "\uD83C\uDCC5"
        case (Diamonds, Six) => "\uD83C\uDCC6"
        case (Diamonds, Seven) => "\uD83C\uDCC7"
        case (Diamonds, Eight) => "\uD83C\uDCC8"
        case (Diamonds, Nine) => "\uD83C\uDCC9"
        case (Diamonds, Ten) => "\uD83C\uDCCA"
        case (Diamonds, Jack) => "\uD83C\uDCCB"
        case (Diamonds, Queen) => "\uD83C\uDCCD"
        case (Diamonds, King) => "\uD83C\uDCCE"

        case (Clubs, Ace) => "\uD83C\uDCD1"
        case (Clubs, Two) => "\uD83C\uDCD2"
        case (Clubs, Three) => "\uD83C\uDCD3"
        case (Clubs, Four) => "\uD83C\uDCD4"
        case (Clubs, Five) => "\uD83C\uDCD5"
        case (Clubs, Six) => "\uD83C\uDCD6"
        case (Clubs, Seven) => "\uD83C\uDCD7"
        case (Clubs, Eight) => "\uD83C\uDCD8"
        case (Clubs, Nine) => "\uD83C\uDCD9"
        case (Clubs, Ten) => "\uD83C\uDCDA"
        case (Clubs, Jack) => "\uD83C\uDCDB"
        case (Clubs, Queen) => "\uD83C\uDCDD"
        case (Clubs, King) => "\uD83C\uDCDE"
    }
}
