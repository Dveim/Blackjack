package cards

import util.Random

class Shoe(n: Int) {
    var cards = getCards
    val sizeToShuffle = cards.length / 3

    def getCards: List[Card] = Random.shuffle({
        for (x <- 1 to n) yield Cards.deck
    }.flatten).toList

    /**
     * Warning! This method changes `cards` as side effect
     */
    def getCard: Card = cards match {
        case x :: xs =>
            cards = xs
            x

        case _ =>
            throw new Exception("Empty shoe, should never happen")
    }

    def checkForShuffle() =
        if (sizeToShuffle > cards.length) {
            println("Shuffling cards...")
            cards = getCards
        }
}
