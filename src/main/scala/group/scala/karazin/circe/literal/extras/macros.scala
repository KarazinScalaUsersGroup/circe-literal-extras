package group.scala.karazin.circe.literal.extras

import java.util.UUID

import cats.implicits._
import cats.data.{Chain, NonEmptyList, NonEmptyVector, OneAnd, Validated}
import cats.data.Validated.{Invalid, Valid}
import io.circe.parser
import io.circe.syntax._
import io.circe.{Json, JsonObject, JsonNumber, Encoder, ACursor, HCursor}

import scala.quoted._
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try as ScalaTry}

object macros:

  object encode:

    sealed trait FreshKey[T <: String]:
      val value: String

    final case class LeftFreshKey(value: String) extends FreshKey["left-fresh-key"]
    final case class RightFreshKey(value: String) extends FreshKey["right-fresh-key"]

    final case class EncodeError(message: String) extends Exception(message)
    final case class EncodeWarning(message: String) extends Exception(message)

    private val UnitUnit = JsonObject.empty
    private val BooleanUnit = true

    private val ByteUnit   = Byte.MaxValue
    private val ShortUnit  = Short.MaxValue
    private val IntUnit    = Int.MaxValue
    private val LongUnit   = Long.MaxValue
    private val FloatUnit  = Float.MaxValue
    private val DoubleUnit = Double.MaxValue
    
    private val CharUnit = ' '
    private val StringUnit = ""

    private val BigIntUnit: BigInt = BigInt(Long.MaxValue) * 10
    private val BigDecimalUnit: BigDecimal = BigDecimal(Double.MaxValue) * 10

    private val UUIDUnit = UUID.randomUUID

    private val JsonObjectUnit = JsonObject.empty
    private val JsonUnit = Json.Null
    private val JsonNumberUnit = JsonNumber.fromString("0").get

    private val DurationUnit = java.time.Duration.ZERO
    private val InstantUnit = java.time.Instant.MIN
    private val PeriodUnit = java.time.Period.ZERO
    private val ZoneIdUnit = java.time.ZoneId.of("+0")
    private val LocalDateUnit = java.time.LocalDate.MIN
    private val LocalTimeUnit = java.time.LocalTime.MIN
    private val LocalDateTimeUnit = java.time.LocalDateTime.MIN
    private val MonthDayUnit = java.time.MonthDay.of(1,1)
    private val OffsetTimeUnit = java.time.OffsetTime.MIN
    private val OffsetDateTimeUnit = java.time.OffsetDateTime.MIN
    private val YearUnit = java.time.Year.now()
    private val YearMonthUnit = java.time.YearMonth.of(0,1)
    private val ZonedDateTimeUnit = java.time.ZonedDateTime.now()
    private val ZoneOffsetUnit = java.time.ZoneOffset.MIN

    private val CurrencyUnit = java.util.Currency.getInstance("USD")
    
    def apply[T](sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])
                (using tpe: Type[T], quotes: Quotes): Expr[Json] =

      import quotes.reflect._

      argsExpr match
        case Varargs(argExprs) =>

          val stringContextParts = getStringContextParts(sc)

          val leftFreshKey: LeftFreshKey = LeftFreshKey(generateFreshKey(stringContextParts))
          val rightFreshKey: RightFreshKey = RightFreshKey(generateFreshKey(stringContextParts))

          val jsonSchema = makeJsonSchema(stringContextParts, deconstructArguments(leftFreshKey, rightFreshKey)(argExprs))

          ScalaTry(validateJsonSchema[T]("*", jsonSchema.hcursor)(using leftFreshKey, rightFreshKey)) match {
            case Success(_) => // intentionally blank

            case Failure(EncodeError(error)) =>
              report.throwError(s"Encode error: [$error]")

            case Failure(EncodeWarning(warning)) =>
              report.warning(s"Encode warning: [$warning]")

            case Failure(error) =>
              report.throwError(s"Unexpected error: [${error.getMessage}]. Cause: [${error.getStackTrace mkString "\n"}]")
          }
          makeJson(sc, argExprs)

        case unexpected =>
           report.throwError(s"Expected Varargs got [$unexpected]")
    
    end apply

    def makeJson(sc: Expr[StringContext], argExprs: Seq[Expr[Any]])(using quotes: Quotes): Expr[Json] =
      import quotes.reflect._

      val argEncodedExprs = argExprs.map {
         case '{ $arg: tp } =>
            TypeRepr.of[tp].widen.asType match
              case '[t] =>  
                Expr.summon[Encoder[t]] match
                   case Some(expr) =>
                     '{ $expr.apply($arg.asInstanceOf[t]) }
                   case None =>
                      report.throwError(s"Could not find implicit for [${Type.show[Encoder[tp]]}]", arg)
      }

      val newArgsExpr = Varargs(argEncodedExprs)

      '{ parser.parse($sc.s($newArgsExpr: _*)).toOption.get }

    end makeJson

    def getStringContextParts(sc: Expr[StringContext])(using quotes: Quotes): List[String] =
      import quotes.reflect._

      '{$sc.parts}.asTerm match

        case Inlined(_, _, Select(Inlined(_, _, Inlined(_, _, Apply(_, List(Typed(Repeated(v, _), _))))), _)) =>
          v map {

            case Literal(StringConstant(part)) =>
              part

            case unexpected =>
              report.throwError(s"Cannot extract string literal from [$unexpected]")
          }

        case unexpected =>
          report.throwError(s"Cannot extract StringContext parts from [$unexpected]")

    end getStringContextParts
    
    def makeJsonSchema(literals: List[String], arguments: List[Json])(using quotes: Quotes): Json =
      import quotes.reflect._

      val jsonString =
        (literals zip arguments).map { case (key, value) =>
          s""" $key ${value.noSpaces} """
        }.mkString("") + literals.last

      parser.parse(jsonString) match
        case Right(json) => json
        case Left(error) => report.throwError(s"Cannot prove json structure for \n$jsonString\n Reason: [$error]")

    end makeJsonSchema   
    
    def deconstructProductFieldNames[T: Type](using Quotes): List[String] = 
      
      Type.of[T] match 
        case '[EmptyTuple]    => List.empty[String]
        case '[head *: tail]  => Type.show[head].stripPrefix("\"").stripSuffix("\"").trim :: deconstructProductFieldNames[tail]
    
    end deconstructProductFieldNames  
    
    def deconstructProductFieldTypes[T: Type](using Quotes): List[Type[_]] =
      import quotes.reflect._

      Type.of[T] match
        case '[EmptyTuple]      => List.empty[Type[_]]
        case '[field *: fields] => Type.of[field] :: deconstructProductFieldTypes[fields]
        case '[tpe]             => report.throwError(s"Macros implementation error. Unexpected type. Required tuple but found [${Type.show[tpe]}]")

    end deconstructProductFieldTypes
    
    def deconstructArgumentProduct[T: Type](using leftFreshKey: LeftFreshKey, rightFreshKey: RightFreshKey)
                                           (using quotes: Quotes): List[(String, Json)] =
      import quotes.reflect._

      val mirrorTpe = Type.of[Mirror.Of[T]]

      Expr.summon(using mirrorTpe) match

        case Some(
              '{
                  $m: Mirror.ProductOf[T] {
                    type MirroredElemLabels = elems
                    type MirroredElemTypes = tpes
                  }
              }
            ) =>

          (deconstructProductFieldNames[elems] zip deconstructProductFieldTypes[tpes]) map {
            case (key, '[Option[tpe]]) =>
              key -> deconstructArgument[tpe]         
                    
            case (key, '[tpe]) =>
              key -> deconstructArgument[tpe]
          }

        case expr =>
          report.throwError(s"Macros implementation error. Unexpected expression. Required Some(Mirror.ProductOf[T]) but found [$expr]")

    end deconstructArgumentProduct
    
    def deconstructArgument[T: Type](using leftFreshKey: LeftFreshKey, rightFreshKey: RightFreshKey)
                                    (using quotes: Quotes): Json =
      import quotes.reflect._
      
      Type.of[T] match
        case '[List[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Seq[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Set[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Vector[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Map[f, t]] if deconstructArgument[f].isString =>
          Json.fromFields((StringUnit, deconstructArgument[t]) :: Nil)
    
        case '[Iterable[t]] =>
          Json.arr(deconstructArgument[t])
    
        case '[Some[t]] =>
          deconstructArgument[t]
    
        case '[None.type] =>
          Json.Null
    
        case '[Option[t]] =>
          deconstructArgument[t]

        case '[Left[t, f]] =>
          Json.fromFields(("Left", deconstructArgument[t]) :: Nil)

        case '[Right[t, f]] =>
          Json.fromFields(("Right", deconstructArgument[f]) :: Nil)

        case '[Either[t, f]] =>
          Json.fromFields((leftFreshKey.value, deconstructArgument[t]) :: (rightFreshKey.value, deconstructArgument[f]) :: Nil)
    
        case '[NonEmptyList[t]] =>
          Json.arr(deconstructArgument[t])
    
        case '[NonEmptyVector[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Chain[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Invalid[t]] =>
          Json.fromFields(("Invalid", deconstructArgument[t]) :: Nil)
    
        case '[Valid[t]] =>
          Json.fromFields(("Valid", deconstructArgument[t]) :: Nil)

        case '[Validated[t, f]] =>
          Json.fromFields((leftFreshKey.value, deconstructArgument[t]) :: (rightFreshKey.value, deconstructArgument[f]) :: Nil)

        case '[tpe] if TypeRepr.of[tpe] <:< TypeRepr.of[Map] =>
          report.throwError(s"Macros does not yet support this type [${Type.show[tpe]}]")
    
        case '[OneAnd[_, _]] =>
          report.throwError(s"Macros does not yet support this type cats.data.OneAnd")

        case '[Unit] =>
          Json.fromJsonObject(UnitUnit)
    
        case '[Boolean] | '[java.lang.Boolean] =>
          Json.fromBoolean(BooleanUnit)

        case '[Byte] | '[java.lang.Byte] =>
          Json.fromInt(ByteUnit)
    
        case '[Short] | '[java.lang.Short] =>
          Json.fromInt(ShortUnit)

        case '[Int] | '[java.lang.Integer] =>
          Json.fromInt(IntUnit)
    
        case '[Long] | '[java.lang.Long]  =>
          Json.fromLong(LongUnit)

        case '[Float] | '[java.lang.Float] =>
          Json.fromFloatOrNull(FloatUnit)

        case '[Double] | '[java.lang.Double] =>
          Json.fromDoubleOrNull(DoubleUnit)

        case '[Char] | '[java.lang.Character] =>
          Json.fromString(CharUnit.toString)

        case '[String] =>
          Json.fromString(StringUnit)

        case '[BigInt] | '[java.math.BigInteger] =>
          Json.fromBigInt(BigIntUnit)

        case '[BigDecimal] | '[java.math.BigDecimal] =>
          Json.fromBigDecimal(BigDecimalUnit)

        case '[java.util.UUID] =>
          Json.fromString(UUIDUnit.toString)

        case '[java.time.Duration] =>
          Json.fromString(DurationUnit.toString)

        case '[java.time.Instant] =>
          Json.fromString(InstantUnit.toString)

        case '[java.time.Period] =>
          Json.fromString(PeriodUnit.toString)

        case '[java.time.ZoneId] =>
          Json.fromString(ZoneIdUnit.getId)

        case '[java.time.LocalDate] =>
          Json.fromString(LocalDateUnit.toString)

        case '[java.time.LocalTime] =>
          Json.fromString(LocalTimeUnit.toString)

        case '[java.time.LocalDateTime] =>
          Json.fromString(LocalDateTimeUnit.toString)

        case '[java.time.MonthDay] =>
          Json.fromString(MonthDayUnit.toString)

        case '[java.time.OffsetTime] =>
          Json.fromString(OffsetTimeUnit.toString)

        case '[java.time.OffsetDateTime] =>
          Json.fromString(OffsetDateTimeUnit.toString)

        case '[java.time.Year] =>
          Json.fromString(YearUnit.toString)

        case '[java.time.YearMonth] =>
          Json.fromString(YearMonthUnit.toString)

        case '[java.time.ZonedDateTime] =>
          Json.fromString(ZonedDateTimeUnit.toString)

        case '[java.time.ZoneOffset] =>
          Json.fromString(ZoneOffsetUnit.toString)

        case '[java.util.Currency] =>
          Json.fromString(CurrencyUnit.toString)

        case '[Json] =>
          JsonUnit

        case '[JsonObject] =>
          Json.fromJsonObject(JsonObjectUnit)

        case '[JsonNumber] =>
          Json.fromJsonNumber(JsonNumberUnit)

        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          Json.fromFields(deconstructArgumentProduct[t])
      
        case '[tpe] =>
          report.throwError(s"Macros implementation error. Unsupported type. Required " +
            s"Unit, Boolean, java.lang.Boolean, Byte, java.lang.Byte,  Short, " +
            s"java.lang.Short, Int, java.lang.Integer, Long, java.lang.Long, Float, java.lang.Float, Double, " +
            s"java.lang.Double, String, Char, java.lang.Character, BigInt, java.math.BigInteger, BigDecimal, " +
            s"java.math.BigDecimal, java.util.UUID, java.time.Duration, java.time.Instant, java.time.Period, " +
            s"java.time.ZoneId, java.time.LocalDate, java.time.LocalTime, java.time.LocalDateTime, java.time.MonthDay," +
            s"java.time.OffsetTime, java.time.OffsetDateTime,java.time.Year, java.time.YearMonth, java.time.ZonedDateTime," +
            s"java.time.ZoneOffset, java.util.Currency, List, Seq, Vector, Map, Set, Iterable, Option, Some, None, Either, " +
            s"cats.data.NonEmptyList, cats.data.NonEmptyVector, cats.data.Chain, cats.data.Validated, " +
            s"io.circe.Json, io.circe.JsonObject, io.circe.JsonNumber, Product but found [${Type.show[tpe]}]")

    end deconstructArgument
    
    def deconstructArguments(leftFreshKey: LeftFreshKey, rightFreshKey: RightFreshKey)
                            (argExprs: Seq[Expr[Any]])(using quotes: Quotes): List[Json] =
      
      argExprs.toList map { 
        case '{ $arg: tpe } => deconstructArgument[tpe](using leftFreshKey, rightFreshKey)
      }
      
    end deconstructArguments
      
    def deconstructTypeSchemaProduct[T: Type](keyPrefix: String, cursor: ACursor)
                                             (using leftFreshKey: LeftFreshKey, rightFreshKey: RightFreshKey)
                                             (using Quotes): Unit =
      import quotes.reflect._

      val mirrorTpe = Type.of[Mirror.Of[T]]

      Expr.summon(using mirrorTpe) match

        case Some(
              '{
                  $m: Mirror.ProductOf[T] {
                    type MirroredElemLabels = elems
                    type MirroredElemTypes = tpes
                  }
              }
            ) =>

          val keys = deconstructProductFieldNames[elems]

          cursor.keys match
            case Some(jsonKeys) => jsonKeys map { key =>
              if !keys.contains(key) then
                throw EncodeWarning(s"""Excess json type by key [$keyPrefix]: an extra field [$key] is found.""")
            }
            case None => // intentionally blank

          (keys zip deconstructProductFieldTypes[tpes]) map {
            case (key, '[Some[tpe]]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => throw EncodeError(s"""Missing required key [${s"$keyPrefix.$key"}]""")

            case (key, '[None.type]) =>
              cursor.downField(key).success match
                case Some(cursor) => throw EncodeError(s"""Unexpected json type by key [$key]. Null is expected""")
                case None         => // intentionally blank

            case (key, '[Option[tpe]]) =>
              cursor.downField(key).success match
                case Some(cursor) if !cursor.value.isNull =>
                  validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case _ => // intentionally blank

            case (key, '[tpe]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => throw EncodeError(s"""Missing required key [${s"$keyPrefix.$key"}]""")
          }
    
        case expr =>
          throw EncodeError(s"Macros implementation error. Unexpected expression. Required Some(Mirror.ProductOf[T]) but found [$expr]")

    end deconstructTypeSchemaProduct
    
    def validateJsonSchema[T: Type](key: String, cursor: HCursor)
                                   (using leftFreshKey: LeftFreshKey, rightFreshKey: RightFreshKey)
                                   (using quotes: Quotes): Unit =
      import quotes.reflect._
      
      Type.of[T] match
        case '[List[t]] => 
          validateJsonArray(key, cursor) { value => 
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[Seq[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[Set[t]] =>
          validateJsonArray(key, cursor, uniqueValuesContainer = true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[Vector[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[Map[f, t]] =>
          validateJsonObject(key, cursor) { key =>
            cursor.downField(key).success match
              case Some(cursor) => validateJsonSchema[t](key, cursor)
              case None         => // intentionally blank
          }
    
        case '[Iterable[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[Chain[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[NonEmptyList[t]] =>
          validateJsonArray(key, cursor, nonEmptyContainer = true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[NonEmptyVector[t]] =>
          validateJsonArray(key, cursor, nonEmptyContainer = true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[tpe] if TypeRepr.of[tpe] <:< TypeRepr.of[Map] =>
          throw EncodeError(s"Macros does not yet support this type [${Type.show[tpe]}]")

        case '[OneAnd[_, _]] =>
          throw EncodeError(s"Macros does not yet support this type cats.data.OneAnd")

        case '[Left[t, _]] =>
          validateJsonSchema[t](key, cursor.downField("Left").success.get)

        case '[Right[_, f]] =>
          validateJsonSchema[f](key, cursor.downField("Right").success.get)

        case '[Either[t, f]] =>
          cursor.keys match
            case Some(keys) if keys.size == 1 && keys.head == "Left" =>
              validateJsonSchema[t](s"$key.Left", cursor.downField("Left").success.get)
            case Some(keys) if keys.size == 1 && keys.head == "Right" =>
              validateJsonSchema[f](s"$key.Right", cursor.downField("Right").success.get)
            case Some(keys) if keys.size == 2 && keys.head == leftFreshKey.value && keys.last == rightFreshKey.value =>
              validateJsonSchema[t](s"$key.${leftFreshKey.value}", cursor.downField(leftFreshKey.value).success.get)
              validateJsonSchema[f](s"$key.${rightFreshKey.value}", cursor.downField(rightFreshKey.value).success.get)
            case keys =>
              throw EncodeError(
                s"""Unexpected json type by key [$key]. Either Left or Right key are expected.
                   |Actual keys are [${cursor.keys map { _ mkString ","}}]""".stripMargin)

        case '[Invalid[t]] =>
          validateJsonSchema[t](key, cursor.downField("Invalid").success.get)

        case '[Valid[t]] =>
          validateJsonSchema[t](key, cursor.downField("Valid").success.get)
      
        case '[Validated[t, f]] =>
          cursor.keys match
            case Some(keys) if keys.size == 1 && keys.head == "Invalid" =>
              validateJsonSchema[t](s"$key.Invalid", cursor.downField("Invalid").success.get)
            case Some(keys) if keys.size == 1 && keys.head == "Valid" =>
              validateJsonSchema[f](s"$key.Valid", cursor.downField("Valid").success.get)
            case Some(keys) if keys.size == 2 && keys.head == leftFreshKey.value && keys.last == rightFreshKey.value =>
              validateJsonSchema[t](s"$key.${leftFreshKey.value}", cursor.downField(leftFreshKey.value).success.get)
              validateJsonSchema[f](s"$key.${rightFreshKey.value}", cursor.downField(rightFreshKey.value).success.get)
            case keys =>
              throw EncodeError(
                s"""Unexpected json type by key [$key]. Validated Invalid or Valid key are expected.
                   |Actual keys are [${cursor.keys map { _ mkString ","}}]""".stripMargin)

        case '[Some[t]] =>
          validateJsonSchema[t](key, cursor)

        case '[None.type] =>
          if (!cursor.value.isNull)
            throw EncodeError(s"""Unexpected json type by key [$key]. Null is expected""")

        case '[Option[t]] =>
          if (!cursor.value.isNull)
            validateJsonSchema[t](key, cursor)

        case  '[Unit] | '[Boolean] | '[java.lang.Boolean] | '[Byte] | '[java.lang.Byte] | '[Short]  | '[java.lang.Short] |
              '[Int] | '[java.lang.Integer] | '[Long] | '[java.lang.Long] | '[Float]  | '[java.lang.Float] |
              '[Double] | '[java.lang.Double] | '[Char] | '[java.lang.Character] | '[String] | '[BigInt] |
              '[java.math.BigInteger] | '[BigDecimal] | '[java.math.BigDecimal] | '[JsonNumber] | '[JsonObject] |
              '[Json] | '[java.util.UUID] | '[java.time.Duration] | '[java.time.Instant] | '[java.time.Period] |
              '[java.time.ZoneId] | '[java.time.LocalDate] | '[java.time.LocalTime] | '[java.time.LocalDateTime] |
              '[java.time.MonthDay] | '[java.time.OffsetTime] | '[java.time.OffsetDateTime] | '[java.time.Year] |
              '[java.time.YearMonth] | '[java.time.ZonedDateTime] | '[java.time.ZoneOffset] | '[java.util.Currency] =>
          validatePrimitives[T](key, cursor)

        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          deconstructTypeSchemaProduct[t](key, cursor)

        case '[unexpected] =>
          report.throwError(s"Macros implementation error. Unsupported type [${Type.show[unexpected]}]")

    end validateJsonSchema

    def validateJsonArray(key: String, cursor: HCursor, nonEmptyContainer: Boolean = false, uniqueValuesContainer: Boolean = false)
                         (f: Json => Any): Unit =
      cursor.focus match
        case Some(json) if json.isArray =>
          cursor.values match
            case Some(values) if nonEmptyContainer && values.isEmpty =>
              throw EncodeError(s"""Unexpected json type by key [$key]. Non-empty json array is expected""")
            case Some(values) if uniqueValuesContainer && values.toSet.size != values.size =>
              throw EncodeError(s"""Unexpected json type by key [$key]. Json array expected with different values""")
            case Some(values) =>
              values map f
            case None => // intentionally blank
        case Some(json) => throw EncodeError(s"""Unexpected json type by key [$key]. Json array is expected""")
        case None       => throw EncodeError(s"""Unexpected json type by key [$key]. Json array is expected""")

    end validateJsonArray

    def validateJsonObject(key: String, cursor: HCursor, nonEmptyContainer: Boolean = false)
                          (f: String => Any): Unit =
      cursor.focus match
        case Some(json) if json.isObject =>
          cursor.keys match
            case Some(keys) if nonEmptyContainer && keys.isEmpty =>
              throw EncodeError(s"""Unexpected json type by key [$key]. Non-empty json object is expected""")
            case Some(keys) if keys.toSet.size != keys.size =>
              throw EncodeError(s"""Unexpected json type by key [$key]. Json object expected with different keys""")
            case Some(keys) =>
              keys map f
            case None => // intentionally blank
        case Some(json) => throw EncodeError(s"""Unexpected json type by key [$key]. Json object is expected""")
        case None       => throw EncodeError(s"""Unexpected json type by key [$key]. Json object is expected""")

    end validateJsonObject
    
    def validatePrimitives[T: Type](key: String, cursor: ACursor)(using Quotes): Unit =
    
      Type.of[T] match
        case '[Unit] =>
          handleError(key, "Unit", cursor.as[Unit])
    
        case '[Boolean] =>
          handleError(key, "Boolean", cursor.as[Boolean])
        
        case '[java.lang.Boolean] =>
          handleError(key, "java.lang.Boolean", cursor.as[java.lang.Boolean])
        
        case '[Int] =>
          handleError(key, "Int", cursor.as[Int])
        
        case '[java.lang.Integer] =>
          handleError(key, "java.lang.Integer", cursor.as[java.lang.Integer])
        
        case '[Short] =>
          handleError(key, "Short", cursor.as[Short])
        
        case '[java.lang.Short] =>
          handleError(key, "java.lang.Short", cursor.as[java.lang.Short])
        
        case '[Long] =>
          handleError(key, "Long", cursor.as[Long])
        
        case '[java.lang.Long] =>
          handleError(key, "java.lang.Long", cursor.as[java.lang.Long])
        
        case '[Byte] =>
          handleError(key, "Byte", cursor.as[Byte])
        
        case '[java.lang.Byte] =>
          handleError(key, "java.lang.Byte", cursor.as[java.lang.Byte])
        
        case '[Float] =>
          handleError(key, "Float", cursor.as[Float])
        
        case '[java.lang.Float] =>
          handleError(key, "java.lang.Float", cursor.as[java.lang.Float])
        
        case '[Double] =>
          handleError(key, "Double", cursor.as[Double])
        
        case '[java.lang.Double] =>
          handleError(key, "java.lang.Double", cursor.as[java.lang.Double])
        
        case '[String] =>
          handleError(key, "String", cursor.as[String])
        
        case '[Char] =>
          handleError(key, "Char", cursor.as[Char])
        
        case '[java.lang.Character] =>
          handleError(key, "java.lang.Character", cursor.as[java.lang.Character])
        
        case '[BigInt] =>
          handleError(key, "BigInt", cursor.as[BigInt])
        
        case '[java.math.BigInteger] =>
          handleError(key, "java.math.BigInteger", cursor.as[java.math.BigInteger])
        
        case '[BigDecimal] =>
          handleError(key, "BigDecimal", cursor.as[BigDecimal])
        
        case '[java.math.BigDecimal] =>
          handleError(key, "java.math.BigDecimal", cursor.as[java.math.BigDecimal])
        
        case '[Json] =>
          handleError(key, "Json", cursor.as[Json])
        
        case '[JsonObject] =>
          handleError(key, "JsonObject", cursor.as[JsonObject])
        
        case '[JsonNumber] =>
          handleError(key, "JsonNumber", cursor.as[JsonNumber])
        
        case '[java.util.UUID] =>
          handleError(key, "java.util.UUID", cursor.as[java.util.UUID])
        
        case '[java.time.Duration] =>
          handleError(key, "java.time.Duration", cursor.as[java.time.Duration])
        
        case '[java.time.Instant] =>
          handleError(key, "java.time.Instant", cursor.as[java.time.Instant])
        
        case '[java.time.Period] =>
          handleError(key, "java.time.Period", cursor.as[java.time.Period])
        
        case '[java.time.ZoneId] =>
          handleError(key, "java.time.ZoneId", cursor.as[java.time.ZoneId])
        
        case '[java.time.LocalDate] =>
          handleError(key, "java.time.LocalDate", cursor.as[java.time.LocalDate])
        
        case '[java.time.LocalTime] =>
          handleError(key, "java.time.LocalTime", cursor.as[java.time.LocalTime])
        
        case '[java.time.LocalDateTime] =>
          handleError(key, "java.time.LocalDateTime", cursor.as[java.time.LocalDateTime])
        
        case '[java.time.MonthDay] =>
          handleError(key, "java.time.MonthDay", cursor.as[java.time.MonthDay])
        
        case '[java.time.OffsetTime] =>
          handleError(key, "java.time.OffsetTime", cursor.as[java.time.OffsetTime])
        
        case '[java.time.OffsetDateTime] =>
          handleError(key, "java.time.OffsetDateTime", cursor.as[java.time.OffsetDateTime])
        
        case '[java.time.Year] =>
          handleError(key, "java.time.Year", cursor.as[java.time.Year])
        
        case '[java.time.YearMonth] =>
          handleError(key, "java.time.YearMonth", cursor.as[java.time.YearMonth])
        
        case '[java.time.ZonedDateTime] =>
          handleError(key, "java.time.ZonedDateTime", cursor.as[java.time.ZonedDateTime])
        
        case '[java.time.ZoneOffset] =>
          handleError(key, "java.time.ZoneOffset", cursor.as[java.time.ZoneOffset])
        
        case '[java.util.Currency] =>
          handleError(key, "java.util.Currency", cursor.as[java.util.Currency])
    
      def handleError(key: String, primitiveType: String, result: Either[Throwable, _]): Unit =
      
        result match
          case Right(_)     => // intentionally blank
          case Left(error)  =>
            throw EncodeError(s"The json does not satisfied to the schema: cannot treat [$key] field as [$primitiveType]. Underline error: [$error]")
      
      end handleError
    
    end validatePrimitives

    def generateFreshKey(stringContextParts: List[String]): String =
      val key = UUID.randomUUID().toString
      if stringContextParts exists { str => str contains key} then generateFreshKey(stringContextParts)
      else key


  end encode

end macros