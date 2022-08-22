package group.scala.karazin.circe.literal.extras.suites

import cats.implicits.*
import group.scala.karazin.circe.literal.extras.macros
import group.scala.karazin.circe.literal.extras.model.ConstantEnum
import io.circe.syntax.*
import io.circe.{Codec, Decoder, Encoder, Json, JsonObject, parser}
import org.scalacheck.*
import org.scalacheck.Prop.*

import scala.compiletime.summonAll
import scala.deriving.Mirror
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

object EnumEncodeSuite:

  object coders:
    inline def stringEnumEncoder[T](using m: Mirror.SumOf[T]): Encoder[T] =
      val elemInstances = summonAll[Tuple.Map[m.MirroredElemTypes, ValueOf]]
        .productIterator.asInstanceOf[Iterator[ValueOf[T]]].map(_.value)
      val elemNames = summonAll[Tuple.Map[m.MirroredElemLabels, ValueOf]]
        .productIterator.asInstanceOf[Iterator[ValueOf[String]]].map(_.value)
      val mapping = (elemInstances zip elemNames).toMap
      Encoder[String].contramap[T](mapping.apply)

    inline def stringEnumDecoder[T](using m: Mirror.SumOf[T]): Decoder[T] =
      val elemInstances = summonAll[Tuple.Map[m.MirroredElemTypes, ValueOf]]
        .productIterator.asInstanceOf[Iterator[ValueOf[T]]].map(_.value)
      val elemNames = summonAll[Tuple.Map[m.MirroredElemLabels, ValueOf]]
        .productIterator.asInstanceOf[Iterator[ValueOf[String]]].map(_.value)
      val mapping = (elemNames zip elemInstances).toMap
      Decoder[String].emap { name =>
        mapping.get(name).fold(Left(s"Name $name is invalid value"))(Right(_))
      }

    given Encoder[ConstantEnum] = stringEnumEncoder[ConstantEnum]
    given Decoder[ConstantEnum] = stringEnumDecoder[ConstantEnum]

class EnumEncodeSuite extends munit.ScalaCheckSuite:
  import EnumEncodeSuite.coders.given

  property("inlined enum value") {

    case class Primitive(value: ConstantEnum) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    lazy val result: Json =
      encode"""
          {
            "value": "Const1"
          }
        """

    val expected: Json =
        parser.parse(
          s"""
        {
          "value": "Const1"
        }
       """
        ).toOption.get

    assertEquals(result, expected)

  }

  property("inlined enum value 2") {

    case class Primitive(value: ConstantEnum) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    lazy val result: Json =
      encode"""
          {
            "value": ${ConstantEnum.Const2}
          }
        """

    val expected: Json =
        parser.parse(
          s"""
        {
          "value": "Const2"
        }
       """
        ).toOption.get

    assertEquals(result, expected)
  }

  property("inlined enum value 3") {

    case class Primitive(value: ConstantEnum) derives Codec.AsObject

    extension (inline sc: StringContext)
      inline def encode(inline args: Any*): Json =
        ${ macros.encode[Primitive]('sc, 'args) }

    val primitive = Primitive(value = ConstantEnum.Const1)

    lazy val result: Json =
      encode"""
          $primitive
        """

    val expected: Json =
      parser.parse(
        s"""
        {
          "value": "Const1"
        }
       """
      ).toOption.get

    assertEquals(result, expected)

  }