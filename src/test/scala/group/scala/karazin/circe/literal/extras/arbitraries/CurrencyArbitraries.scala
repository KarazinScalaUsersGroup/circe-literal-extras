package group.scala.karazin.circe.literal.extras.arbitraries

import cats.implicits._
import io.circe.syntax._
import io.circe.parser
import io.circe.{Json, JsonObject}
import org.scalacheck._
import org.scalacheck.Prop._
import java.util.Currency
import scala.jdk.CollectionConverters._

trait CurrencyArbitraries {

  val currencies = Currency.getAvailableCurrencies.asScala

  given Arbitrary[Currency] = Arbitrary(Gen.pick(1, currencies).map(_.head))

}
