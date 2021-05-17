package group.scala.karazin.circe.literal.extras.arbitraries

import io.circe.testing.instances.arbitraryJsonObject
import io.circe.JsonObject
import org.scalacheck.Arbitrary

trait JsonArbitraries {

  given Arbitrary[JsonObject] = arbitraryJsonObject

}
