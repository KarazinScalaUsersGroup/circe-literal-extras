package group.scala.karazin.circe.literal.extras.list

import cats.implicits._
import group.scala.karazin.circe.literal.extras.arbitraries.{given, _}
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Codec, Encoder, Json, JsonObject}
import org.scalacheck._

object model:
  case class Foo(ints: List[Int], options: List[Option[Boolean]]) derives Codec.AsObject
  case class FooLike(ints: List[Int], options: List[Boolean]) derives Codec.AsObject
  
  val genFoo: Gen[Foo] = for {
    ints     <- Arbitrary.arbitrary[List[Int]]
    options  <- Arbitrary.arbitrary[List[Option[Boolean]]]
  } yield Foo(ints, options)

  val genFooLike: Gen[FooLike] = for {
    ints     <- Arbitrary.arbitrary[List[Int]]
    options  <- Arbitrary.arbitrary[List[Boolean]]
  } yield FooLike(ints, options)
  
  given Arbitrary[Foo] = Arbitrary(genFoo)
  given Arbitrary[FooLike] = Arbitrary(genFooLike)

  given tToJson[T: Encoder]: Conversion[T, Json] = summon[Encoder[T]](_)
  given optionToJson[T: Encoder]: Conversion[Option[T], Json] = summon[Encoder[Option[T]]](_)
  given listToJson[T: Encoder]: Conversion[List[T], Json] = summon[Encoder[List[T]]](_)
