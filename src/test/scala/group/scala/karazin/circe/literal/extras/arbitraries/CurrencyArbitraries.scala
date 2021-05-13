package group.scala.karazin.circe.literal.extras.arbitraries

import org.scalacheck._
import java.util.Currency
import scala.jdk.CollectionConverters._

trait CurrencyArbitraries {

  val currencies = Currency.getAvailableCurrencies.asScala

  given Arbitrary[Currency] = Arbitrary(Gen.oneOf(currencies))

}
