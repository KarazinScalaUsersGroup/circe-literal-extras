package group.scala.karazin.circe.literal.extras.generators

import cats.implicits._
import io.circe.syntax._
import io.circe.testing.instances.arbitraryJsonObject
import io.circe.{Json, JsonNumber, JsonObject, parser}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import io.circe.Encoder.encodeJsonObject

/**
  * as an option, we can put all our json generation methods (incl. custom ones if need be)
  * in a separate file, like here
  */
object JsonGenerators {

  val genJsonObjectDefault: Gen[JsonObject] =
    arbitraryJsonObject.arbitrary.sample.get

}
