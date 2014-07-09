package states

import cards._
import Event.getEvent
import io.StdIn.readInt

/**
 * Main class for FSM. Combination of case class and trait instead of single trait
 * is used to get some benefits like copy() and therefore reduce code duplication
 */

case class State(cash: Int,
                 bet: Int,
                 shoe: Shoe,
                 playerCards: List[Card],
                 dealerCards: List[Card]) {

    def this(s: State) = this(s.cash, s.bet, s.shoe, s.playerCards, s.dealerCards)

    def getBet: Int = {
        println("Please, enter your bet")
        val _bet = readInt()
        println()
        require(1 <= _bet && _bet <= cash)
        _bet
    }

    def getScore(cards: List[Card]): Int = {
        val (aces, other) = cards.partition(_.rank == Ace)
        val sumOfOther = other.map(_.rank match {
            case Two => 2
            case Three => 3
            case Four => 4
            case Five => 5
            case Six => 6
            case Seven => 7
            case Eight => 8
            case Nine => 9
            case _ => 10 // ace can't be here
        }).sum
        aces.foldLeft(sumOfOther)((res, _) => if (res < 11) res + 11 else res + 1)
    }
}

/**
 * Handles FSM states changes
 */

sealed trait Transition extends State {
    def _transit: Transition

    def stateEvent: Event

    def printCards(cards: List[Card], message: String) {
        println(message)
        cards foreach (x => print(s"$x  "))
        print(s"with total score ${getScore(cards)}\n")
    }

    def printInfo() {
        printCards(playerCards, "Your cards:")
        printCards(dealerCards, "Dealer cards:")
        println(s"Your cash - $cash, current bet - $bet\n")
    }

    def transit = {
        printInfo()
        shoe.checkForShuffle()
        _transit
    }
}

trait StartS extends Transition {
    override def printCards(cards: List[Card], message: String) {} // for smart printing

    override def stateEvent = Start

    // only bet is possible at this state
    override def _transit = {
        try {
            val _bet = getBet
            val playerCard1 = shoe.getCard
            val dealerCard = shoe.getCard
            val playerCard2 = shoe.getCard

            // blackjack can be only in start
            if (getScore(playerCard1 :: playerCard2 :: Nil) == 21) {
                println(s"Blackjack! $playerCard1  $playerCard2")
                new State(cash + 3 * _bet / 2, 0, shoe, Nil, Nil) with StartS
            }

            else new State(
                cash - _bet,
                bet + _bet,
                shoe,
                playerCard1 :: playerCard2 :: playerCards,
                dealerCard :: dealerCards) with DealS {
                override def isAfterStart: Boolean = true // can split only right after the start
            }

        } catch {
            case e@(_: NumberFormatException | _: IllegalArgumentException) =>
                println(s"Please, enter a valid 1 <= integer <= $cash")
                this
        }
    }
}

trait DealS extends Transition {
    override def stateEvent = Deal

    def isAfterStart = false

    override def _transit = getEvent match {
        case Hit =>
            val newPlayerCard = shoe.getCard
            println(s"You get $newPlayerCard")
            val newPlayerCards = newPlayerCard :: playerCards
            val playerScore = getScore(newPlayerCards)

            if (playerScore > 21) {
                println("You busted")
                new State(cash, 0, shoe, Nil, Nil) with StartS
            }
            else new State(copy(playerCards = newPlayerCards)) with DealS

        case Stand =>
            new State(copy()) with StandS

        case Double =>
            if (cash < bet) {
                println("Sorry, not enough money for double")
                this
            }

            else if (getScore(playerCards) > 10) {
                println("Sorry, you can double only if score < 11")
                this
            }

            else {
                val newPlayerCard = shoe.getCard
                println(s"You get $newPlayerCard,  total score is ${getScore(newPlayerCard :: playerCards)}")
                new State(cash - bet, 2 * bet, shoe, newPlayerCard :: playerCards, dealerCards) with StandS
            }

        case Split =>
            if (cash < bet) {
                println("Sorry, not enough money for split")
                this
            }

            else if (playerCards.length != 2 || (playerCards(0).rank != playerCards(1).rank)) {
                println("Sorry, you can split only with two cards of same ranks")
                this
            }

            else if (!isAfterStart) {
                println("Sorry, you can split only right after the start, multiple splits are not allowed")
                this
            }

            else new State(copy()) with SplitS

        case _ =>
            println("You should press `hit`, `stand`, `double` or `split` at this phase")
            this
    }
}

trait StandS extends Transition {
    override def stateEvent = Stand

    override def printInfo() {} // for smart printing

    override def _transit = {
        val dealerScore = getScore(dealerCards)

        // dealer hits again
        if (dealerScore < 17) {
            val dealerCard = shoe.getCard
            val newDealerCards = dealerCard :: dealerCards
            println(s"Dealer got $dealerCard , his new score is ${getScore(newDealerCards)}")

            new State(copy(dealerCards = newDealerCards)) with StandS
        }

        // dealer stands (not busted)
        else if (dealerScore < 22) {
            val playerScore = getScore(playerCards)

            if (playerScore > dealerScore) {
                println("You win")
                new State(cash + 2 * bet, 0, shoe, Nil, Nil) with StartS
            }

            else if (playerScore < dealerScore) {
                println("You lose")
                new State(cash, 0, shoe, Nil, Nil) with StartS
            }

            else {
                println("Nobody wins or loses")
                new State(cash + bet, 0, shoe, Nil, Nil) with StartS
            }
        }

        // dealer busts
        else {
            println("Dealer busts")
            new State(cash + 3 * bet / 2, 0, shoe, Nil, Nil) with StartS
        }
    }
}

trait SplitS extends Transition {
    override def stateEvent = Split

    override def printInfo() {} // for smart printing

    def fillHand(cash: Int, bet: Int, shoe: Shoe, playerCards: List[Card], dealerCards: List[Card]) = {
        var game: Transition = new State(cash, bet, shoe, playerCards, dealerCards) with DealS
        while (game.stateEvent == Deal)
            game = game.transit
        game.playerCards
    }

    override def _transit = {
        println("Current hand splits")
        val _cash = cash - bet

        // atm playerCards has exactly 2 elements
        // fill left and right hands

        println("Left hand")
        val leftCards = fillHand(_cash, 2 * bet, shoe, List(shoe.getCard, playerCards(0)), dealerCards)

        println("Right hand")
        val rightCards = fillHand(_cash, 2 * bet, shoe, List(shoe.getCard, playerCards(1)), dealerCards)

        // dealers takes cards
        // dealer doesn't look at player cards, so let's start from left hand
        var game: Transition = new State(0, bet, shoe, leftCards, dealerCards) with StandS
        var newDealerCards: List[Card] = dealerCards
        println("Left hand result:")
        while (game.stateEvent == Stand) {
            newDealerCards = game.dealerCards
            game = game.transit
        }
        val leftProfit = game.cash

        // now right hand, also it is guaranteed that dealer won't take new cards
        game = new State(0, bet, shoe, rightCards, newDealerCards) with StandS
        print("Right hand result - ")
        val rightProfit = game.transit.cash

        new State(_cash + leftProfit + rightProfit, 0, shoe, Nil, Nil) with StartS
    }
}
