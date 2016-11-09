import scala.util.{ Try, Failure, Success }

object Test extends App {
  println(Parser.parse("true"))
  println(Parser.parse("'string'"))
  println(Parser.parse("'string'extra"))
  println(Parser.parse("['str']"))
}

trait JsonObject
case class JsonBoolean(value: Boolean) extends JsonObject
case class JsonString(value: String) extends JsonObject
case class JsonArray(value: Seq[JsonObject]) extends JsonObject

object Parser {
  def parse(input: String): Try[JsonObject] =
    BooleanParser.parse(input) orElse
      StringParser.parse(input) orElse
      Failure(new Exception(s"Could not parse $input"))
}

object BooleanParser {
  def parse(input: String): Try[JsonBoolean] = input.toLowerCase match {
    case Bool(bool) => Success(JsonBoolean(bool))
    case other => Failure(new Exception(s"Unrecognized input to parse boolean: $other"))
  }
}

object StringParser {
  val Str = "'([^']*)'(.*)".r

  def parse(input: String): Try[JsonString] = input match {
    case Str(str, rest) => Success(JsonString(str))
    case other => Failure(new Exception(s"Unrecognized input to parse string: $other"))
  }
}


// val l: List[Char]
// l match {
//   case '\' :: next :: rest => continue('\', next)
//   case '\'' rest => done
//   case char => continue(char)
// }

// object ArrayParser {
//   def parse(input: String): Try[JsonArray] = input match {
//     case Str(str) => Success(JsonString(str))
//     case other => Failure(new Exception(s"Unrecognized input to parse string: $other"))
//   }
// }


object Bool {
  def unapply(str: String): Option[Boolean] = str match {
    case "true" => Some(true)
    case "false" => Some(false)
    case _ => None
  }
}
