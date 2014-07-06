package cards

import util.Random

class Shoe(n: Int) {
  var cards = getCards
  val sizeToShuffle = cards.length / 3

  private def getCards: List[Card] = Random.shuffle({
    for (x <- 1 to n) yield Cards.deck
  }.flatten).toList

  def getCard: Card = cards match {
    case x :: xs =>
      if (xs.length > sizeToShuffle) { //todo fix unnormal shiffle
        cards = xs
        x
      } else {
        cards = getCards
        getCard
      }

    case _ =>
      throw new Exception("Empty shoe, should never happen")
  }
}
