import cards._
import states._

object Main {
  def main(args: Array[String]) = {
    var game: State with Transition = new State(100, 0, new Shoe(4), Nil, Nil) with StartS
    //todo write rules (dealer bust = 0.5, win = 1)

    while (!(game.cash == 0 && game.bet == 0)) {
      game = game.transit
    }

    println("Game over")
  }
}
