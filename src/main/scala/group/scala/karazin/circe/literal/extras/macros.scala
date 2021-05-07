package group.scala.karazin.circe.literal.extras

import cats.implicits._
import cats.data.{Chain, NonEmptyChain, NonEmptyList, NonEmptyMap, NonEmptySet, NonEmptyVector, OneAnd, Validated}
import io.circe.parser
import io.circe.syntax._
import io.circe.{ACursor, Encoder, HCursor, Json, JsonObject}

import scala.quoted._
import scala.deriving.Mirror
import scala.util.{Failure, Success, Try as UTry}

object macros:

  object encode:

    final case class EncodeException(private val message: String = "") extends Exception(message)

    private val BooleanUnit = true
    private val IntUnit = 0
    private val StringUnit = ""
    private val JsonObjectUnit = JsonObject.empty
    
    def apply[T](sc: Expr[StringContext], argsExpr: Expr[Seq[Any]])
                (using tpe: Type[T], quotes: Quotes): Expr[Json] =

      import quotes.reflect._

      argsExpr match
        case Varargs(argExprs) =>
          
          val jsonSchema = makeJsonSchema(getStringContextParts(sc), deconstructArguments(argExprs))
          UTry(validateJsonSchema("*", jsonSchema.hcursor)) match {
            case Success(_)                    => // intentionally blank
            case Failure(EncodeException(exc)) => report.throwError(s"$exc")
            case Failure(exc) => throw exc
          }
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

        case '[Seq[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Set[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Vector[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Map[f, t]] if deconstructArgument[f].isString =>
          Json.fromFields((StringUnit, deconstructArgument[t]) :: Nil)

        case '[NonEmptyList[t]] =>
          Json.arr(deconstructArgument[t])
    
        case '[NonEmptyVector[t]] =>
          Json.arr(deconstructArgument[t])

        case '[NonEmptySet[t]] =>
          Json.arr(deconstructArgument[t])

        case '[NonEmptyMap[f, t]] if deconstructArgument[f].isString =>
          Json.fromFields((StringUnit, deconstructArgument[t]) :: Nil)

        case '[Chain[t]] =>
          Json.arr(deconstructArgument[t])
    
        case '[NonEmptyChain[t]] =>
          Json.arr(deconstructArgument[t])

        case '[Validated[t, f]] =>
          Json.fromFields((StringUnit, deconstructArgument[t]) :: Nil)
    
        case '[Either[t, f]] =>
          Json.fromFields((StringUnit, deconstructArgument[f]) :: Nil)

        case '[Some[t]] =>
          deconstructArgument[t]

        case '[None.type] =>
          Json.Null
    
        case '[Option[t]] => 
          deconstructArgument[t]
    
        case '[JsonObject] =>
          Json.fromJsonObject(JsonObjectUnit)

        case '[Iterable[t]] =>
          Json.arr(deconstructArgument[t])

        case '[tpe] if TypeRepr.of[tpe] <:< TypeRepr.of[Map] =>
          report.throwError(s"Macros does not yet support this type `${Type.show[tpe]}`")
    
        case '[OneAnd[_, _]] =>
          report.throwError(s"Macros does not yet support this type cats.data.OneAnd")
    
        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          Json.fromFields(deconstructArgumentProduct[t])      
    
        case '[Boolean] => 
          Json.fromBoolean(BooleanUnit)
    
        case '[Int] =>
          Json.fromInt(IntUnit)
    
        case '[String] =>
          Json.fromString(StringUnit)
      
        case '[tpe] =>
          report.throwError(s"Macros implementation error. Unsupported type. Required List, Seq, Vector, Map, Set, Iterable, " +
            s"cats.data.NonEmptyList, cats.data.NonEmptyVector, cats.data.NonEmptySet, cats.data.NonEmptyMap, " +
            s"cats.data.Chain, cats.data.NonEmptyChain, cats.data.Validated, Either, Option, Some, None, Product, " +
            s"Boolean, Int, String but found `${Type.show[tpe]}`")
      
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

          (deconstructProductFieldNames[elems] zip deconstructProductFieldTypes[tpes]) map {
            case (key, '[Some[tpe]]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => throw EncodeException(s"""Missing required key `${s"$keyPrefix.$key"}`""")

            case (key, '[None.type]) =>
              cursor.downField(key).success match
                case Some(cursor) => throw EncodeException(s"""Unexpected json type by key `$key`. Null is expected""")
                case None         => // intentionally blank

            case (key, '[Option[tpe]]) =>
              cursor.downField(key).success match
                case Some(cursor) if !cursor.value.isNull =>
                  validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case _ => // intentionally blank

            case (key, '[tpe]) =>
              cursor.downField(key).success match
                case Some(cursor) => validateJsonSchema[tpe](s"$keyPrefix.$key", cursor)
                case None         => throw EncodeException(s"""Missing required key `${s"$keyPrefix.$key"}`""")
          }
    
        case expr =>
          throw EncodeException(s"Macros implementation error. Unexpected expression. Required Some(Mirror.ProductOf[T]) but found `$expr`")

    end deconstructTypeSchemaProduct
    
    def validateJsonSchema[T: Type](key: String, cursor: HCursor)(using Quotes): Unit =
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
          validateJsonArray(key, cursor, false, true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[Vector[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[Chain[t]] =>
          validateJsonArray(key, cursor) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[Map[f, t]] =>
          validateJsonObject(key, cursor) { key => 
            cursor.downField(key).success match
              case Some(cursor) => validateJsonSchema[t](key, cursor)
              case None         => // intentionally blank
        }

        case '[NonEmptyList[t]] =>
          validateJsonArray(key, cursor, true) { value => 
            validateJsonSchema[t](key, value.hcursor)
          }

        case '[NonEmptyVector[t]] =>
          validateJsonArray(key, cursor, true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[NonEmptySet[t]] =>
          validateJsonArray(key, cursor, true, true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[NonEmptyMap[f, t]] =>
          validateJsonObject(key, cursor, true) { key =>
            cursor.downField(key).success match
              case Some(cursor) => validateJsonSchema[t](key, cursor)
              case None         => // intentionally blank
          }
    
        case '[NonEmptyChain[t]] =>
          validateJsonArray(key, cursor, true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[tpe] if TypeRepr.of[tpe] <:< TypeRepr.of[Map] =>
          report.throwError(s"Macros does not yet support this type `${Type.show[tpe]}`")
    
        case '[OneAnd[_, _]] =>
          report.throwError(s"Macros does not yet support this type cats.data.OneAnd")
    
        case '[Either[t, f]] =>
          UTry(validateJsonSchema[t](key, cursor)) match {
            case Success(_)   => // intentionally blank
            case Failure(exc) => validateJsonSchema[f](key, cursor)
          }
 
        case '[Validated[t, f]] =>
          UTry(validateJsonSchema[t](key, cursor)) match {
            case Success(_)   => // intentionally blank
            case Failure(exc) => validateJsonSchema[f](key, cursor)
        }

        case '[Some[t]] =>
          validateJsonSchema[t](key, cursor)

        case '[None.type] =>
          if (!cursor.value.isNull) 
            throw EncodeException(s"""Unexpected json type by key `$key`. Null is expected""")
    
        case '[Option[t]] =>
          if (!cursor.value.isNull) 
            validateJsonSchema[t](key, cursor)

        case '[Iterable[t]] =>
          validateJsonArray(key, cursor, true) { value =>
            validateJsonSchema[t](key, value.hcursor)
          }
    
        case '[Boolean] | '[Int] | '[String] | '[JsonObject] => 
          validatePrimitives[T](key, cursor)

        case '[t] if TypeRepr.of[t] <:< TypeRepr.of[Product] =>
          deconstructTypeSchemaProduct[t](key, cursor)      

        case '[unexpected] =>
          throw EncodeException(s"Macros implementation error. Unsupported type `${Type.show[unexpected]}`")
    
    end validateJsonSchema
    
    def validateJsonArray(key: String, cursor: HCursor, nonEmpty: Boolean = false, differentValues: Boolean = false)
                         (f: Json => Any): Unit =
      
      cursor.focus match
        case Some(json) if json.isArray =>
          cursor.values match
            case Some(values) if nonEmpty && values.isEmpty =>
              throw EncodeException(s"""Unexpected json type by key `$key`. Non-empty json array is expected""")
            case Some(values) if differentValues && values.toSet.size == values.size =>
              throw EncodeException(s"""Unexpected json type by key `$key`. Json array expected with different values""")
            case Some(values) =>
              values map f
            case None => // intentionally blank
        case Some(json) => throw EncodeException(s"""Unexpected json type by key `$key`. Json array is expected""")
        case None       => throw EncodeException(s"""Unexpected json type by key `$key`. Json array is expected""")

    end validateJsonArray

    def validateJsonObject(key: String, cursor: HCursor, nonEmpty: Boolean = false)
                          (f: String => Any): Unit =
      
      cursor.focus match
        case Some(json) if json.isObject =>
          cursor.keys match
            case Some(keys) if nonEmpty && keys.isEmpty =>
              throw EncodeException(s"""Unexpected json type by key `$key`. Non-empty json object is expected""")
            case Some(keys) if keys.toSet.size == keys.size =>
              throw EncodeException(s"""Unexpected json type by key `$key`. Json object expected with different keys""")
            case Some(keys) =>
              keys map f
            case None => // intentionally blank
        case Some(json) => throw EncodeException(s"""Unexpected json type by key `$key`. Json object is expected""")
        case None       => throw EncodeException(s"""Unexpected json type by key `$key`. Json object is expected""")

    end validateJsonObject

    def validatePrimitives[T: Type](key: String, cursor: ACursor)(using Quotes): Unit =
      
      def handleError(key: String, primitiveType: String, result: Either[Throwable, _]): Unit =
        result match
          case Right(_)     => // intentionally blank
          case Left(error)  => throw EncodeException(s"The json does not sitisfied to the schema: cannot treat `$key` field as `$primitiveType`. Underline error: `$error`")
      end handleError
      
      Type.of[T] match
        case '[Boolean] =>
          handleError(key, "Boolean", cursor.as[Boolean])
    
        case '[Int] =>
          handleError(key, "Int", cursor.as[Int])
    
        case '[String] =>
          handleError(key, "String", cursor.as[String])
    
        case '[JsonObject] =>
          handleError(key, "JsonObject", cursor.as[JsonObject])

    end validatePrimitives
    
  end encode

end macros