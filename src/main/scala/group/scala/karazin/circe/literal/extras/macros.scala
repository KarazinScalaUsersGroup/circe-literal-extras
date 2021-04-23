package group.scala.karazin.circe.literal.extras

import cats.implicits.{given, _}
import io.circe.parser
import io.circe.syntax._
import io.circe.{Json, JsonObject, JsonNumber, Encoder, ACursor, HCursor}

import scala.quoted._

import scala.deriving.Mirror

object macros:

  object encode:

    private val BooleanUnit = true
    private val StringUnit = ""
    private val UUIDUnit = ""
    private val CharUnit = ' '
    private val IntUnit = 0.toInt
    private val ShortUnit = 0.toShort
    private val ByteUnit = 0.toByte
    private val LongUnit = 0.toLong
    private val BigIntUnit = new BigInt(java.math.BigInteger.valueOf(0))
    private val BigDecimalUnit = new BigDecimal(java.math.BigDecimal.valueOf(0))
    private val DoubleUnit = 0.0.toDouble
    private val FloatUnit = 0.0.toFloat
    private val UnitUnit = JsonObject.empty
    private val JsonObjectUnit = JsonObject.empty
    private val JsonUnit = Json.Null
    private val JsonNumberUnit = JsonNumber.fromString("0").get
    
    def apply[T](sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])
                (using tpe: Type[T], quotes: Quotes): Expr[Json] =

      import quotes.reflect._

      argsExpr match
        case Varargs(argExprs) =>
          
          val jsonSchema = makeJsonSchema(getStringContextParts(sc), deconstructArguments(argExprs))
          validateJsonSchema("*", jsonSchema.hcursor)
          makeJson(sc, argExprs)

        case unexpected =>
           // `new StringContext(...).showMeExpr(args: _*)` not an explicit `showMeExpr"..."`
           report.throwError(s"Expected Varargs got `$unexpected`")
    
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
                      report.throwError(s"Could not find implicit for `${Type.show[Encoder[tp]]}``", arg)
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
              report.throwError(s"Cannot extract string literal from `$unexpected`")
          }

        case unexpected =>
          report.throwError(s"Cannot extract StringContext parts from `$unexpected`")

    end getStringContextParts
    
    def makeJsonSchema(literals: List[String], arguments: List[Json])(using quotes: Quotes): Json =
      import quotes.reflect._

      val jsonString =
        (literals zip arguments).map { case (key, value) =>
          s""" $key ${value.noSpaces} """
        }.mkString("") + literals.last

      parser.parse(jsonString) match
        case Right(json) => json
        case Left(error) => report.throwError(s"Cannot prove json structure for \n$jsonString\n Reason: `$error`")

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
        case '[tpe]             => report.throwError(s"Macros implementation error. Unexpected type. Required tuple but found `${Type.show[tpe]}``")

    end deconstructProductFieldTypes
    
    def deconstructArgumentProduct[T: Type](using Quotes): List[(String, Json)] =
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
          report.throwError(s"Macros implementation error. Unexpected expression. Required Some(Mirror.ProductOf[T]) but found `$expr`")

    end deconstructArgumentProduct
    
    def deconstructArgument[T: Type](using quotes: Quotes): Json =
      import quotes.reflect._
      
      Type.of[T] match
        case '[List[t]] =>
          Json.arr(deconstructArgument[t])
    
        case '[Option[t]] => 
          deconstructArgument[t]
    
        case '[JsonObject] =>
          Json.fromJsonObject(JsonObjectUnit)

        case '[Json] =>
          JsonUnit

        case '[JsonNumber] =>
          Json.fromJsonNumber(JsonNumberUnit)
    
        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          Json.fromFields(deconstructArgumentProduct[t])    
    
        case '[Boolean] | '[java.lang.Boolean] =>
          Json.fromBoolean(BooleanUnit)

        case '[Byte] | '[java.lang.Byte] =>
          Json.fromInt(ByteUnit.toInt)
    
        case '[Int] | '[java.lang.Integer] =>
          Json.fromInt(IntUnit)

        case '[Short] | '[java.lang.Short] =>
          Json.fromInt(ShortUnit.toInt)

        case '[Float] | '[java.lang.Float] =>
          Json.fromFloatOrNull(FloatUnit)

        case '[Double] | '[java.lang.Double] =>
          Json.fromDoubleOrNull(DoubleUnit)

        case '[Long] | '[java.lang.Long] =>
          Json.fromLong(LongUnit)

        case '[String] =>
          Json.fromString(StringUnit)

        case '[Char] | '[java.lang.Character] =>
          Json.fromString(CharUnit.toString)

        case '[BigInt] | '[java.math.BigInteger] =>
          Json.fromBigInt(BigIntUnit)

        case '[BigDecimal] | '[java.math.BigDecimal] =>
          Json.fromBigDecimal(BigDecimalUnit)

        case '[java.util.UUID] =>
          Json.fromString(UUIDUnit)

        case '[Unit] =>
          Json.fromJsonObject(UnitUnit)
      
        case '[tpe] =>
          report.throwError(s"Macros implementation error. Unsupported type. Required List, Option, Product, " +
            s"Boolean, java.lang.Boolean, Int, java.lang.Integer, String, Char, java.lang.Character, Short, " +
            s"java.lang.Short, Byte, java.lang.Byte, Long, java.lang.Long, Float, java.lang.Float, Double, " +
            s"java.lang.Double, BigInt, java.math.BigInteger, BigDecimal, java.math.BigDecimal, java.util.UUID" +
            s"Unit but found `${Type.show[tpe]}`")
      
    end deconstructArgument     
    
    def deconstructArguments(argExprs: Seq[Expr[Any]])(using quotes: Quotes): List[Json] =
      
      argExprs.toList map { 
        case '{ $arg: tpe } => deconstructArgument[tpe]
      }
      
    end deconstructArguments
      
    def deconstructTypeSchemaProduct[T: Type](keyPrefix: String, cursor: ACursor)(using Quotes): Unit =
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

          (deconstructProductFieldNames[elems] zip deconstructProductFieldTypes[tpes]) foreach {
            case (key, '[Option[tpe]]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => // intentionally blank

            case (key, '[tpe]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => report.throwError(s"""Missing required key `${s"$keyPrefix.$key"}`""")
          }

        case expr =>
          report.throwError(s"Macros implementation error. Unexpected expression. Required Some(Mirror.ProductOf[T]) but found `$expr`")

    end deconstructTypeSchemaProduct
    
    def validateJsonSchema[T: Type](key: String, cursor: HCursor)(using Quotes): Unit =
      import quotes.reflect._
      
      Type.of[T] match
        case '[List[t]] =>
          cursor.focus match
            case Some(json) if json.isArray =>
              cursor.values match
                case Some(values) =>
                  values foreach { value =>
                    validateJsonSchema[t](key, value.hcursor)
                  }

                case None         => // intentionally blank   
              
            case Some(json) =>
              report.throwError(s"""Unexpected json type by key `$key`. Json array is expected""")
              
            case None => 
              report.throwError(s"""Unexpected json type by key `$key`. Json array is expected""")
          
        case '[Option[t]] => 
          validateJsonSchema[t](key, cursor)
    
        case  '[Boolean] | '[java.lang.Boolean]       |
              '[Long] | '[java.lang.Long]             |
              '[Int]    | '[java.lang.Integer]        |
              '[Short]  | '[java.lang.Short]          |
              '[Byte]   | '[java.lang.Byte]           |
              '[Double] | '[java.lang.Double]         |
              '[Float]  | '[java.lang.Float]          |
              '[Char]   | '[java.lang.Character]      |
              '[BigInt] | '[java.math.BigInteger]     |
              '[BigDecimal] | '[java.math.BigDecimal] |
              '[JsonObject] | '[Json] | '[JsonNumber] |
              '[String] | '[java.util.UUID] | '[Unit] =>
          validatePrimitives[T](key, cursor)

        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          deconstructTypeSchemaProduct[t](key, cursor)      

        case '[unexpected] =>
          report.throwError(s"Macros implementation error. Unsupported type `${Type.show[unexpected]}`")
    
      def validatePrimitives[T: Type](key: String, cursor: ACursor)(using Quotes): Unit = 
        
        Type.of[T] match
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

          case '[Float] =>
            handleError(key, "Float", cursor.as[Float])

          case '[java.lang.Float] =>
            handleError(key, "java.lang.Float", cursor.as[java.lang.Float])

          case '[Double] =>
            handleError(key, "Double", cursor.as[Double])

          case '[java.lang.Double] =>
            handleError(key, "java.lang.Double", cursor.as[java.lang.Double])

          case '[Long] =>
            handleError(key, "Long", cursor.as[Long])

          case '[java.lang.Long] =>
            handleError(key, "java.lang.Long", cursor.as[java.lang.Long])

          case '[String] =>
            handleError(key, "String", cursor.as[String])

          case '[Char] =>
            handleError(key, "Char", cursor.as[Char])

          case '[java.lang.Character] =>
            handleError(key, "java.lang.Character", cursor.as[java.lang.Character])

          case '[Byte] =>
            handleError(key, "Byte", cursor.as[Byte])

          case '[java.lang.Byte] =>
            handleError(key, "java.lang.Byte", cursor.as[java.lang.Byte])

          case '[BigInt] =>
            handleError(key, "BigInt", cursor.as[BigInt])

          case '[java.math.BigInteger] =>
            handleError(key, "java.math.BigInteger", cursor.as[java.math.BigInteger])

          case '[BigDecimal] =>
            handleError(key, "BigDecimal", cursor.as[BigDecimal])

          case '[java.math.BigDecimal] =>
            handleError(key, "java.math.BigDecimal", cursor.as[java.math.BigDecimal])
    
          case '[JsonObject] => 
            handleError(key, "JsonObject", cursor.as[JsonObject])

          case '[Json] =>
            handleError(key, "Json", cursor.as[Json])

          case '[JsonNumber] =>
            handleError(key, "JsonNumber", cursor.as[JsonNumber])

          case '[java.util.UUID] =>
            handleError(key, "java.util.UUID", cursor.as[java.util.UUID])

          case '[Unit] =>
            handleError(key, "Unit", cursor.as[Unit])
        
        def handleError(key: String, promitiveType: String, result: Either[Throwable, _]): Unit = 
          
          result match
            case Right(_)     => // intentionally blank
            case Left(error)  => report.throwError(s"The json does not sitisfied to the schema: cannot treat `$key` field as `$promitiveType`. Underline error: `$error`")
        
        end handleError
    
      end validatePrimitives
    
    end validateJsonSchema

  end encode

end macros