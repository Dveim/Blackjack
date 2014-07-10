import cards._
import states._

object Main {
    def main(args: Array[String]) = {
        println("Some rules:")
        println(" - shoe has 4 decks")
        println(" - blackjack gives 1.5 bet")
        println(" - you get 0.5 bet if dealer busts")
        println(" - you can split only if both cards have same ranks (no Ace + Ten, only Ace + Ace)")
        println(" - Ace values 11 if score < 11, else 1 (Ace + Ace + Ten == 22)")
        println(" - dealer can't blackjack")
        println(" - you can double if score < 11")
        println(" - you can double only right after the start")
        println("Good luck!\n")

        var game: Transition = new State(100, 0, new Shoe(4), Nil, Nil) with StartS
        while (!(game.cash == 0 && game.bet == 0))
            game = game.transit

        println("Game over")
    }
}
