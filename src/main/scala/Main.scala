import cards._
import states._

object Main {
    def main(args: Array[String]) = {
        var game: Transition = new State(100, 0, new Shoe(4), Nil, Nil) with StartS
        //todo write rules (dealer bust = you get 0.5 bet, win = 1 bet, split only if same ranks, ace+ace+ten=22)

        while (!(game.cash == 0 && game.bet == 0))
            game = game.transit

        println("Game over")
    }
}
