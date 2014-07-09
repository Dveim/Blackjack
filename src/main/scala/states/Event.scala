package states

/**
 * Duplicate string input to objects to get some tips from compiler
 */

sealed trait Event

object Event {
    def getEvent: Event = io.StdIn.readLine("Please, make your move \n").toLowerCase match {
        case "start" => Start
        case "deal" => Deal
        case "hit" => Hit
        case "stand" => Stand
        case "split" => Split
        case _ => NotRecognized
    }
}

object Start extends Event
object Deal extends Event
object Hit extends Event
object Stand extends Event
object Split extends Event
object NotRecognized extends Event
