package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Json, JsonObject, Encoder, Codec}

object model:
  case class Buzz(int: Int, bool: Boolean, short: Short) derives Codec.AsObject
  case class BuzzLike(int: Int, bool: Boolean, short: Short) derives Codec.AsObject
  case class Bar(str: String, bool: Boolean, unit: Unit) derives Codec.AsObject
  case class BarLike(str: String, bool: Boolean, unit: Unit) derives Codec.AsObject
  case class Foo(int: Int, bar: Option[Bar], buzzes: List[Buzz], qux: JsonObject) derives Codec.AsObject
  case class FooLike(int: Int, bar: Bar, buzzes: List[Buzz], qux: JsonObject) derives Codec.AsObject

  given tToJson[T: Encoder]: Conversion[T, Json] = summon[Encoder[T]](_)
  given optionToJson[T: Encoder]: Conversion[Option[T], Json] = summon[Encoder[Option[T]]](_)
  given listToJson[T: Encoder]: Conversion[List[T], Json] = summon[Encoder[List[T]]](_)

