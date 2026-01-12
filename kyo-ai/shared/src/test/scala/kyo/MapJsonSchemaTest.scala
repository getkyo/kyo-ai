package kyo

import kyo.ai.json.*

class MapJsonSchemaTest extends Test:

    "JsonSchema for Maps" - {
        "Map[String, Int] generates valid schema" in run {
            val schema = Json.jsonSchema[Map[String, Int]]

            schema match
                case JsonSchema.JsonArraySchema(tpe, _, _, items, _, _, _) =>
                    assert(tpe == "array")
                    items match
                        case JsonSchema.JsonObjectSchema(objType, _, _, props, required, _, _) =>
                            assert(objType == "object")
                            assert(props.size == 2)
                            assert(props.contains("key"))
                            assert(props.contains("value"))
                            assert(required == List("key", "value"))

                            // Verify key schema
                            props("key") match
                                case JsonSchema.JsonStringSchema(_, _, _, _, _, _, _) => succeed
                                case _                                                => fail("Expected string schema for key")

                            // Verify value schema
                            props("value") match
                                case JsonSchema.JsonNumberSchema(_, _, _, _, _, _, _, _, _) => succeed
                                case _                                                      => fail("Expected number schema for value")

                        case _ => fail("Expected JsonObjectSchema for items")
                    end match
                case _ => fail("Expected JsonArraySchema")
            end match
        }

        "Map[Int, String] generates valid schema" in run {
            val schema = Json.jsonSchema[Map[Int, String]]

            schema match
                case JsonSchema.JsonArraySchema(_, _, _, items, _, _, _) =>
                    items match
                        case JsonSchema.JsonObjectSchema(_, _, _, props, _, _, _) =>
                            // Verify key schema
                            props("key") match
                                case JsonSchema.JsonNumberSchema(_, _, _, _, _, _, _, _, _) => succeed
                                case _                                                      => fail("Expected number schema for key")

                            // Verify value schema
                            props("value") match
                                case JsonSchema.JsonStringSchema(_, _, _, _, _, _, _) => succeed
                                case _                                                => fail("Expected string schema for value")

                        case _ => fail("Expected JsonObjectSchema for items")
                case _ => fail("Expected JsonArraySchema")
            end match
        }

        "schema can be encoded to JSON" in run {
            val schema       = Json.jsonSchema[Map[String, Int]]
            val schemaJson   = Json.encode(schema)
            val schemaPretty = Json.encodePretty(schema)

            // Verify it's valid JSON
            assert(schemaJson.contains("\"type\""))
            assert(schemaJson.contains("\"array\""))
            assert(schemaJson.contains("\"properties\""))
            assert(schemaPretty.length > schemaJson.length) // Pretty version is longer

            println("Schema JSON:")
            println(schemaPretty)
            succeed
        }

        "ZIO uses object format for encoding" in run {
            // ZIO's default Map encoding uses object format {"key": value}
            // This is different from the schema we send to LLMs (array format)
            val map     = Map("a" -> 1, "b" -> 2)
            val encoded = Json.encode(map)

            // Verify ZIO encodes as object
            assert(encoded.contains("\"a\""))
            assert(encoded.contains("\"b\""))

            // And can decode its own format
            val decoded = Json.decode[Map[String, Int]](encoded).getOrThrow
            assert(decoded == map)
        }

        "nested Map in case class" in run {
            case class Container(id: Int, metadata: Map[String, String]) derives Json, CanEqual

            val schema = Json.jsonSchema[Container]

            // Verify the schema structure
            schema match
                case JsonSchema.JsonObjectSchema(_, _, _, props, _, _, _) =>
                    assert(props.contains("id"))
                    assert(props.contains("metadata"))

                    // Check that metadata is an array schema
                    props("metadata") match
                        case JsonSchema.JsonArraySchema(_, _, _, _, _, _, _) => succeed
                        case _                                               => fail("Expected array schema for metadata Map")

                case _ => fail("Expected JsonObjectSchema for Container")
            end match
            // Test encoding/decoding
            val original = Container(42, Map("name" -> "test", "version" -> "1.0"))
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Container](encoded).getOrThrow
            assert(decoded == original)
        }

        "Map with complex value type" in run {
            case class Person(name: String, age: Int) derives Json, CanEqual

            val schema = Json.jsonSchema[Map[String, Person]]

            schema match
                case JsonSchema.JsonArraySchema(_, _, _, items, _, _, _) =>
                    items match
                        case JsonSchema.JsonObjectSchema(_, _, _, props, _, _, _) =>
                            props("value") match
                                case JsonSchema.JsonObjectSchema(_, _, _, personProps, _, _, _) =>
                                    assert(personProps.contains("name"))
                                    assert(personProps.contains("age"))
                                case _ => fail("Expected object schema for Person value")
                        case _ => fail("Expected JsonObjectSchema for items")
                case _ => fail("Expected JsonArraySchema")
            end match
            // Test round-trip
            val original = Map("alice" -> Person("Alice", 30), "bob" -> Person("Bob", 25))
            val encoded  = Json.encode(original)
            val decoded  = Json.decode[Map[String, Person]](encoded).getOrThrow
            assert(decoded == original)
        }
    }
end MapJsonSchemaTest
