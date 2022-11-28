package h4

// Similar to e33, implement a simple JSON serializer using type-classes
// Make sure the statements in the main can be executed. The sample output is given in comments. You can do the indentation
// as you like.

class PhoneNo(val prefix: Int, val number: Int)

object PhoneNo:
  given JsonSerializer[PhoneNo] with
    def serialize(p: PhoneNo) =
      import JsonSerializer.given
      f"{ \"prefix\": ${p.prefix.toJson}, \"number\": ${p.number.toJson} }"

class Person(val firstName: String, val lastName: String, val phone: PhoneNo)

object Person:
  given JsonSerializer[Person]  with
    def serialize(p: Person) =
      import JsonSerializer.given
      f"{ \"firstName\": ${p.firstName.toJson}, \"lastName\": ${p.lastName.toJson}, \"phone\": ${summon[JsonSerializer[PhoneNo]].toJson(p.phone)} }"


class Address(val person: Person, val street: String, val city: String)

object Address:
  given JsonSerializer[Address]  with
    def serialize(a: Address) =
      import JsonSerializer.given
      f"{ \"person\": ${summon[JsonSerializer[Person]].toJson(a.person)}, \"street\": ${a.street}, \"city\": ${a.city} }"

trait JsonSerializer[T]:
  def serialize(obj: T): String

  extension (x: T) // alows me to call .toYaml on type T
    def toJson: String = serialize(x)

object JsonSerializer:
  // create implicit parameter fro each type I wanne serialize. It implements trait YamlSerializer[TypeToSerialize]
  given stringSerializer: JsonSerializer[String] with
    def serialize(s: String) = f"\"${s}\""

  given intSerializer:JsonSerializer[Int] with
    def serialize(i : Int) = i.toString()

  given listSerializer[T](using JsonSerializer[T]/* takes implicit param*/): JsonSerializer[List[T]] with
    def serialize(lst: List[T]) =
      val sb = new StringBuilder("[ ")
      var first = true
      for entry <- lst yield
        val value = summon[JsonSerializer[T]].toJson(entry) // summon[TYPE] returns implicit param of type TYPE
        if(!first)
          sb.append(", ") 
        first = false 
        sb.append(value)
        
      sb.append(" ]")
      sb.toString()

  given mapSerializer[T](using JsonSerializer[T]) :JsonSerializer[Map[String,T]] with
    def serialize(map : Map[String, T]) =
      val sb = new StringBuilder("{ ")
      var first = true
      for (key, value) <- map yield
        val keyJson = key.toJson
        val valJson = summon[JsonSerializer[T]].toJson(value)
        if(!first)
          sb.append(", ") 
        first = false
        sb.append(f"${keyJson}: ${valJson}")
      sb.append(" }")
      sb.toString()


object JsonSerializerTest:
  def main(args: Array[String]): Unit =
    import JsonSerializer.given
    val a1 = "Hello"
    println(a1.toJson) // "Hello"

    val a2 = 12
    println(a2.toJson) // 12

    val b1 = List("ab", "cd")
    val b2 = List("ef", "gh")
    println(b1.toJson) // [ "ab", "cd" ]

    val c1 = List(b1, b2)
    println(c1.toJson) // [ [ "ab", "cd" ], [ "ef", "gh" ] ]

    val c2 = Map("b1" -> b1, "b2" -> b2)
    println(c2.toJson) // { "b1": [ "ab", "cd" ], "b2": [ "ef", "gh" ] }

    val p = PhoneNo(1, 123456)
    println(p.toJson)

    val d1 = Person("John", "Doe", PhoneNo(1, 123456))
    val d2 = Person("Jane", "X", PhoneNo(420, 345678))
    println(d1.toJson) // { "firstName": "John", "lastName": "Doe", "phone": { "prefix": 1, "number": 123456 } }

    val e1 = Address(d1, "Bugmore Lane 3", "Lerfourche")
    val e2 = Address(d2, "West End Woods 1", "Holmefefer")

    val f = List(e1, e2)
    println(f.toJson) // [ { "person": { "firstName": "John", "lastName": "Doe", "phone": { "prefix": 1, "number": 123456 } }, "street": "Bugmore Lane 3", "city": "Lerfourche" }, { "person": { "firstName": "Jane", "lastName": "X", "phone": { "prefix": 420, "number": 345678 } }, "street": "West End Woods 1", "city": "Holmefefer" } ]
