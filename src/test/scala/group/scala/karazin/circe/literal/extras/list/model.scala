package group.scala.karazin.circe.literal.extras.list

import cats.implicits._
import group.scala.karazin.circe.literal.extras.arbitraries.{given, _}
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Codec, Encoder, Json, JsonObject}
import org.scalacheck._

object model:
  case class Bar(str: String) derives Codec.AsObject
  case class Foo(ints: List[Int], options: List[Option[Boolean]], bars: List[Bar]) derives Codec.AsObject
  case class FooLike(ints: List[Int], options: List[Boolean], bars: List[Bar]) derives Codec.AsObject

  val genBar: Gen[Bar] = for {
    str   <- Arbitrary.arbitrary[String]
  } yield Bar(str)
  
  val genFoo: Gen[Foo] = for {
    ints     <- Arbitrary.arbitrary[List[Int]]
    options  <- Arbitrary.arbitrary[List[Option[Boolean]]]
    bars     <- Arbitrary.arbitrary[List[Bar]]
  } yield Foo(ints, options, bars)

  val genFooLike: Gen[FooLike] = for {
    ints     <- Arbitrary.arbitrary[List[Int]]
    options  <- Arbitrary.arbitrary[List[Boolean]]
    bars     <- Arbitrary.arbitrary[List[Bar]]
  } yield FooLike(ints, options, bars)

  given Arbitrary[Bar] = Arbitrary(genBar)
  given Arbitrary[Foo] = Arbitrary(genFoo)
  given Arbitrary[FooLike] = Arbitrary(genFooLike)

  given tToJson[T: Encoder]: Conversion[T, Json] = summon[Encoder[T]](_)
  given optionToJson[T: Encoder]: Conversion[Option[T], Json] = summon[Encoder[Option[T]]](_)
  given listToJson[T: Encoder]: Conversion[List[T], Json] = summon[Encoder[List[T]]](_)
