package group.scala.karazin.circe.literal.extras

import org.scalacheck._
import org.scalacheck.Prop._
import io.circe.parser
import io.circe.syntax._
import io.circe.{Json, JsonObject, Encoder, Codec}
import group.scala.karazin.circe.literal.extras.arbitraries.instances.{given, _}

class SetEncodeSuite extends munit.ScalaCheckSuite:
  property("inlined Set of Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[Int]]('sc, 'args)}

    forAll { (set: Set[Int]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Set of Option Int values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[Option[Int]]]('sc, 'args)}

    forAll { (set: Set[Option[Int]]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Set of primitives with Int values") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[Primitive]]('sc, 'args)}

    forAll { (set: Set[Primitive]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }


  property("inlined Set of Option primitives") {

    case class Primitive(value: Int) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[Int].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[Option[Primitive]]]('sc, 'args)}

    forAll { (set: Set[Option[Primitive]]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Set of JsonObject values") {

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[JsonObject]]('sc, 'args)}

    forAll { (set: Set[JsonObject]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }

  property("inlined Set of primitives with JsonObject values") {

    case class Primitive(value: JsonObject) derives Codec.AsObject

    given Arbitrary[Primitive] = Arbitrary(Arbitrary.arbitrary[JsonObject].map(Primitive(_)))

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${macros.encode[Set[Primitive]]('sc, 'args)}

    forAll { (set: Set[Primitive]) =>

      val result: Json = encode"$set"

      val expected: Json = set.asJson

      assertEquals(result, expected)

    }

  }
