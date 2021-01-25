# circe-literal-extras

Compile time literal json validator supporting inlined values based on type schema.

An extra module for JSON library for Scala [circe](https://github.com/circe/circe). Supported Scala versions include 3.0.0-M3.

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
            
val buzzes: List[Buzz] = ???

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
   
val bar = ???

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


val fooInlinedJson: Json =
encode"""
        $foo
        """
```

It is not required to use the same types as defined in type schema. The types which preserve the type schema are also accepted:

```scala
case class BuzzLike(int: Int, bool: Boolean)
case class BarLike(str: String, bool: Boolean)
case class FooLike(int: Int, bar: Option[BarLike], buzzes: List[BuzzLike])

...

encode"""
        $fooLike
        """
```

# License
circe-literal-extras is licensed under the [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0) (the "License"); you may not use this software except in compliance with the License.

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.



