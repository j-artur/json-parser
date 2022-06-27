pub mod json;

#[cfg(test)]
mod test {
    use crate::json::Json;

    #[test]
    fn test_playground() {
        let json_string =
        "{     \"foo\": \"bar\",    \"baz\": 2.0,    \"arr\": [    null,     true,false,    [  ]  ] ,   \"obj\"  :  {  }}";

        match Json::parse_json(&json_string) {
            Some(json) => println!("{}", json),
            None => println!("Invalid JSON"),
        }
    }

    #[test]
    fn test_null() {
        assert_eq!(Json::parse_json("null"), Some(Json::Null));
    }

    #[test]
    fn test_bool() {
        assert_eq!(Json::parse_json("true"), Some(Json::Bool(true)));
        assert_eq!(Json::parse_json("false"), Some(Json::Bool(false)));
    }

    #[test]
    fn test_string() {
        assert_eq!(
            Json::parse_json("\"str\""),
            Some(Json::String("str".to_owned()))
        );
        assert_eq!(Json::parse_json("\"\""), Some(Json::String("".to_owned())));
    }

    #[test]
    fn test_number() {
        assert_eq!(Json::parse_json("0"), Some(Json::Number(0.0)));
        assert_eq!(Json::parse_json("000"), None);
        assert_eq!(Json::parse_json("+2"), None);
        assert_eq!(Json::parse_json("02"), None);
        assert_eq!(Json::parse_json("100"), Some(Json::Number(100.0)));
        assert_eq!(Json::parse_json("-0.50"), Some(Json::Number(-0.5)));
    }

    #[test]
    fn test_array() {
        let array_is_empty = Json::parse_json(" [] ").map_or(false, |j| match j {
            Json::Array(array) => array.len() == 0,
            _ => false,
        });
        assert!(array_is_empty);

        let array_has_null = Json::parse_json("[ null ]").map_or(false, |j| match j {
            Json::Array(array) => array.contains(&Json::Null),
            _ => false,
        });
        assert!(array_has_null);

        let array_has_only_boolean =
            Json::parse_json("[ true , false ]").map_or(false, |j| match j {
                Json::Array(array) => {
                    !array.contains(&Json::Null)
                        && array.contains(&Json::Bool(false))
                        && array.contains(&Json::Bool(true))
                }
                _ => false,
            });
        assert!(array_has_only_boolean);

        let array_has_only_string =
            Json::parse_json("[ \"foo\" , \"bar\" ]").map_or(false, |j| match j {
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
        let object_is_empty = Json::parse_json(" {  } ").map_or(false, |j| match j {
            Json::Object(object) => object.len() == 0,
            _ => false,
        });
        assert!(object_is_empty);

        let object_has_foo_bar =
            Json::parse_json("{ \"foo\": \"bar\" }").map_or(false, |j| match j {
                Json::Object(object) => object.get("foo").map_or(false, |value| match value {
                    Json::String(string) => string == "bar",
                    _ => false,
                }),
                _ => false,
            });
        assert!(object_has_foo_bar);

        let object_has_only_string =
            Json::parse_json("{ \"foo\": [ \"bar\" , \"baz\" ] }").map_or(false, |j| match j {
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
