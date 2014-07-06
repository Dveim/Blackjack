package states

sealed trait Event

object Event {
  def getEvent = Console.in.readLine.toLowerCase match {
    case "start" => Start
    case "deal" => Deal
    case _ => NotRecognized
  }
}

object Start extends Event

object Deal extends Event

object NotRecognized extends Event
