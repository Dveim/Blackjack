package states

/**
 * Duplicate string input to objects to get some tips from compiler
 */

sealed trait Event

object Event {
  def getEvent = io.StdIn.readLine("Please, make your move : \n").toLowerCase match {
    case "start" => Start
    case "deal" => Deal
    case _ => NotRecognized
  }
}

object Start extends Event

object Deal extends Event

object NotRecognized extends Event
