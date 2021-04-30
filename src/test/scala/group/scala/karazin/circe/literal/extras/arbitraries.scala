package group.scala.karazin.circe.literal.extras

import cats.implicits._
import org.scalacheck._
import io.circe.syntax._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Codec, Encoder, Json, JsonObject}

object arbitraries {
  val genBool: Gen[Boolean] = Gen.oneOf(true, false)
  val genStr: Gen[String] = Gen.alphaStr

  val genJsonObject: Gen[JsonObject] = for {
    map <- Arbitrary.arbitrary[Map[String, String]]
  } yield JsonObject.fromMap(map collect {
    case (key, value) if key.nonEmpty => key -> Json.fromString(value)
  })

  given Arbitrary[Boolean] = Arbitrary(genBool)
  given Arbitrary[String] = Arbitrary(genStr)
  given Arbitrary[JsonObject] = Arbitrary(genJsonObject)
}
  

