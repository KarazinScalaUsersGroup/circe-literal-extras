package group.scala.karazin.circe.literal.extras

import cats.implicits._
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Json, Encoder, ParsingFailure, ACursor, HCursor}

object model:
  case class Buzz(int: Int, bool: Boolean)
  case class BuzzLike(int: Int, bool: Boolean)
  case class Bar(str: String, bool: Boolean)
  case class BarLike(str: String, bool: Boolean)
  case class Foo(int: Int, bar: Option[Bar], buzzes: List[Buzz])
  case class FooLike(int: Int, bar: Option[Bar], buzzes: List[Buzz])

  given Encoder[Bar] = deriveEncoder[Bar]
  given Encoder[BarLike] = deriveEncoder[BarLike]
  given Encoder[Buzz] = deriveEncoder[Buzz]
  given Encoder[BuzzLike] = deriveEncoder[BuzzLike]
  given Encoder[Foo] = deriveEncoder[Foo]
  given Encoder[FooLike] = deriveEncoder[FooLike]

  given tToJson[T: Encoder]: Conversion[T, Json] = summon[Encoder[T]](_)
  given optionToJson[T: Encoder]: Conversion[Option[T], Json] = summon[Encoder[Option[T]]](_)
  given listToJson[T: Encoder]: Conversion[List[T], Json] = summon[Encoder[List[T]]](_)

