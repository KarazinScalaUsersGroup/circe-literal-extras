package group.scala.karazin.circe.literal.extras.arbitraries

import cats.implicits._
import io.circe.syntax._
import io.circe.testing.instances.arbitraryJsonObject
import io.circe.{Json, JsonNumber, JsonObject, parser}
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import io.circe.Encoder.encodeJsonObject

trait JsonArbitraries {

  given Arbitrary[JsonObject] = arbitraryJsonObject

}
