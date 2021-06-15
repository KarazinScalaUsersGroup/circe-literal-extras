# circe-literal-extras

Compile time literal json validator supporting inlined values based on type schema.

An extra module for JSON library for Scala [circe](https://github.com/circe/circe). Supported Scala versions include 3.0.0-RC3.

# 🔨 How to build

To be done

# ⭐ Sonatype artifact

To be done

# 👀 Quick example of usage
Here's a quick example of circe-literal-extras in action:

Define custom type schema (in a separate file):
```scala
case class Buzz(int: Int, bool: Boolean)
case class Bar(str: String, bool: Boolean)
case class Foo(int: Int, bar: Option[Bar], buzzes: List[Buzz])
```

Define custom string interpolation prefix:
```scala
import group.scala.karazin.circe.literal.extras._

extension (inline sc: StringContext)
  inline def encode(inline args: Any*): Json = // Custom "encode" string interpolation prefix
    ${ macros.encode[Foo]('sc, 'args) } // Macros with `Foo` type schema
```

The following interpolated strings are successfully verified on compile time. `circe` json encoders for inlined values are required. 
```scala


val rawJson: Json =
    encode"""
          {
            "int": 42,
            "bar": {
              "str": "str",
              "bool": true
             },
            "buzzes": [
              {
                "int": 42,
                "bool": true
              },
              {
                "int": 42,
                "bool": false
              }
            ]
          }
          """
          

val bool: Boolean = ...

val booleanInlinedJson: Json =
    encode"""
            {
              "int": 42,
              "bar": {
                "str": "str",
                "bool": $bool
               },
              "buzzes": [
                {
                  "int": 42,
                  "bool": $bool
                },
                {
                  "int": 42,
                  "bool": $bool
                }
              ]
            }
            """
            

val int: Int = ...

val intInlinedJson: Json =
    encode"""
            {
              "int": $int,
              "bar": {
                "str": "str",
                "bool": true
               },
              "buzzes": [
                {
                  "int": $int,
                  "bool": true
                },
                {
                  "int": $int,
                  "bool": false
                }
              ]
            }
            """
           

val str: String = ...

val stringInlinedJson: Json =
    encode"""
            {
              "int": 42,
              "bar": {
                "str": $str,
                "bool": true
               },
              "buzzes": [
                {
                  "int": 42,
                  "bool": true
                },
                {
                  "int": 42,
                  "bool": false
                }
              ]
            }
            """
            
val buzzes: List[Buzz] = ...

val buzzesInlinedJson: Json =
    encode"""
            {
              "int": 42,
              "bar": {
                "str": "str",
                "bool": true
               },
              "buzzes": $buzzes
            }
            """
   
val bar: Bar = ...

val barInlinedJson: Json =
    encode"""
            {
              "int": 42,
              "bar": $bar,
              "buzzes": [
                {
                  "int": 42,
                  "bool": true
                },
                {
                  "int": 42,
                  "bool": false
                }
              ]
            }
            """

val foo: Foo = ...

val fooInlinedJson: Json =
    encode"""
            $foo
            """
```

It is not required to use the same types as defined in type schema. The types which preserve the type schema are also accepted:

```scala
case class BuzzLike(int: Int, bool: Boolean)
case class BarLike(str: String, bool: Boolean)
case class FooLike(int: Int, bar: BarLike, buzzes: List[BuzzLike]) // `bar` is required 

...

val fooLike: FooLike = ...

val fooLikeInlinedJson: Json =
    encode"""
            $fooLike
            """
```

# 🎯 Supported types
The macros supports the following primitive types:
* Unit
* Boolean
* java.lang.Boolean 
* Byte
* java.lang.Byte
* Short
* java.lang.Short
* Int
* java.lang.Integer
* Long
* java.lang.Long
* Float
* java.lang.Float
* Double
* java.lang.Double
* String, Char
* java.lang.Character
* BigInt
* java.math.BigInteger
* BigDecimal
* java.math.BigDecimal
* java.util.UUID
* java.time.Duration
* java.time.Instant
* java.time.Period
* java.time.ZoneId
* java.time.LocalDate
* java.time.LocalTime
* java.time.LocalDateTime
* java.time.MonthDay
* java.time.OffsetTime
* java.time.OffsetDateTime
* java.time.Year
* java.time.YearMonth
* java.time.ZonedDateTime
* java.time.ZoneOffset
* java.util.Currency

The macros supports the following container types:
* List
* Seq
* Vector
* Map
* Set
* Iterable
* Option
* Some
* None
* Either
* cats.data.NonEmptyList
* cats.data.NonEmptyVector
* cats.data.Chain
* cats.data.Validated

Also the macro supports:
* Product
* Union types
* io.circe.Json
* io.circe.JsonObject
* io.circe.JsonNumber

# 👍 Contribute
Contributions are welcome!

If you want to say thank you, add a [GitHub Star](https://github.com/KarazinScalaUsersGroup/circe-literal-extras/stargazers) to the project.

# 📜 Contributors
[<img src="https://github.com/IgorWolkov.png" width="50"></img>](https://github.com/IgorWolkov)
[![](https://github.com/mikhailZakharevich.png?size=50)](https://github.com/mikhailZakharevich)
[![](https://github.com/VLADISLAV008.png?size=50)](https://github.com/VLADISLAV008)
[<img src="https://github.com/shvedchenkoM.png" width="50"></img>](https://github.com/shvedchenkoM)

# ⚠ License
circe-literal-extras is licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the "License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.



