pub mod json;

#[cfg(test)]
mod test {
    use crate::json::{Json, JsonParser};

    #[test]
    fn test_playground() {
        let playground = || {
            let json_string = "\"a\\na\"";

            println!("debug string: {:?}", json_string);
            println!("string: {}", json_string);
            let json = JsonParser::parse(&json_string);
            println!("json: {:?}", json);
            match json {
                Some(value) => {
                    println!("value: {:?}", value.string());
                }
                None => todo!(),
            }

            Some(())
        };

        assert_eq!(playground(), Some(()));
    }

    #[test]
    fn test_null() {
        assert_eq!(JsonParser::parse("null"), Some(Json::Null));
    }

    #[test]
    fn test_bool() {
        assert_eq!(JsonParser::parse("true"), Some(Json::Bool(true)));
        assert_eq!(JsonParser::parse("false"), Some(Json::Bool(false)));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            JsonParser::parse("\"str\""),
            Some(Json::String("str".to_owned()))
        );
        assert_eq!(JsonParser::parse("\"\""), Some(Json::String("".to_owned())));
        assert_eq!(
            JsonParser::parse("\"\\t\""),
            Some(Json::String("\t".to_owned()))
        );
        assert_eq!(
            JsonParser::parse("\"\\n\\r\\t\\\\\\/\""),
            Some(Json::String("\n\r\t\\/".to_owned()))
        );
        assert_eq!(
            JsonParser::parse("\"\\\"a\\\"\""),
            Some(Json::String("\"a\"".to_owned()))
        );
    }

    #[test]
    fn test_number() {
        assert_eq!(JsonParser::parse("0"), Some(Json::Number(0.0)));
        assert_eq!(JsonParser::parse("000"), None);
        assert_eq!(JsonParser::parse("+2"), None);
        assert_eq!(JsonParser::parse("02"), None);
        assert_eq!(JsonParser::parse("100"), Some(Json::Number(100.0)));
        assert_eq!(JsonParser::parse("-0.50"), Some(Json::Number(-0.5)));
    }

    #[test]
    fn test_array() {
        let array_is_empty = JsonParser::parse(" [] ").map_or(false, |j| match j {
            Json::Array(array) => array.len() == 0,
            _ => false,
        });
        assert!(array_is_empty);

        let array_has_null = JsonParser::parse("[ null ]").map_or(false, |j| match j {
            Json::Array(array) => array.contains(&Json::Null),
            _ => false,
        });
        assert!(array_has_null);

        let array_has_only_boolean =
            JsonParser::parse("[ true , false ]").map_or(false, |j| match j {
                Json::Array(array) => {
                    !array.contains(&Json::Null)
                        && array.contains(&Json::Bool(false))
                        && array.contains(&Json::Bool(true))
                }
                _ => false,
            });
        assert!(array_has_only_boolean);

        let array_has_only_string =
            JsonParser::parse("[ \"foo\" , \"bar\" ]").map_or(false, |j| match j {
                Json::Array(array) => {
                    array
                        .iter()
                        .filter(|json| match json {
                            Json::String(_) => false,
                            _ => true,
                        })
                        .collect::<Vec<&Json>>()
                        .len()
                        == 0
                }
                _ => false,
            });
        assert!(array_has_only_string);
    }

    #[test]
    fn test_object() {
        let object_is_empty = JsonParser::parse(" {  } ").map_or(false, |j| match j {
            Json::Object(object) => object.len() == 0,
            _ => false,
        });
        assert!(object_is_empty);

        let object_has_foo_bar =
            JsonParser::parse("{ \"foo\": \"bar\" }").map_or(false, |j| match j {
                Json::Object(object) => object.get("foo").map_or(false, |value| match value {
                    Json::String(string) => string == "bar",
                    _ => false,
                }),
                _ => false,
            });
        assert!(object_has_foo_bar);

        let object_has_only_string = JsonParser::parse("{ \"foo\": [ \"bar\" , \"baz\" ] }")
            .map_or(false, |j| match j {
                Json::Object(object) => object.get("foo").map_or(false, |value| match value {
                    Json::Array(array) => {
                        array
                            .iter()
                            .filter(|json| match json {
                                Json::String(string) => string != ("bar") && string != ("baz"),
                                _ => true,
                            })
                            .collect::<Vec<&Json>>()
                            .len()
                            == 0
                    }
                    _ => false,
                }),
                _ => false,
            });
        assert!(object_has_only_string);
    }
}
