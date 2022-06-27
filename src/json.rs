use std::{collections::HashMap, fmt::Display};

pub type ParseResult<'s, T> = (&'s str, T);

type JsonArray = Vec<Json>;
type JsonObject = HashMap<String, Json>;

#[derive(Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    String(String),
    Number(f64),
    Array(JsonArray),
    Object(JsonObject),
}

pub struct JsonParser;

impl JsonParser {
    fn parse_char(src: &str, c: char) -> Option<ParseResult<char>> {
        (c == src.chars().nth(0)?).then(|| Some((src.get(1..)?, c)))?
    }

    fn parse_char_if<F: Fn(char) -> bool>(src: &str, f: F) -> Option<ParseResult<char>> {
        let c = src.chars().nth(0)?;
        f(c).then(|| Some((src.get(1..)?, c)))?
    }

    fn parse_chars<'s>(src: &'s str, str: &str) -> Option<ParseResult<'s, &'s str>> {
        if src.len() < str.len() {
            return None;
        }

        let mut chars: &str = "";
        let mut rest = src;

        for (i, c) in str.chars().enumerate() {
            let (str, _) = JsonParser::parse_char(rest, c)?;
            chars = src.get(..i + 1)?;
            rest = str;
        }

        Some((rest, chars))
    }

    fn parse_chars_while<F: Fn(char) -> bool>(src: &str, f: F) -> Option<ParseResult<&str>> {
        let mut buffer: &str = "";
        let mut rest = src;

        for (i, _) in src.chars().enumerate() {
            let c = rest.chars().nth(0)?;
            let str = rest.get(1..)?;

            if !f(c) {
                break;
            }
            buffer = match src.get(..i + 1) {
                Some(str) => str,
                None => break,
            };
            rest = str;
        }

        Some((rest, buffer))
    }

    fn parse_string_literal(src: &str) -> Option<ParseResult<String>> {
        let (rest, _) = JsonParser::parse_char(src, '\"')?;
        let (rest, string) = JsonParser::parse_chars_while(rest, |c| c != '\"')?; // Can't escape characters and can break line
        let (rest, _) = JsonParser::parse_char(rest, '\"')?;

        Some((rest, String::from(string)))
    }

    fn parse_null(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "null")?;
        Some((rest, Json::Null))
    }

    fn parse_true(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "true")?;
        Some((rest, Json::Bool(true)))
    }

    fn parse_false(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "false")?;
        Some((rest, Json::Bool(false)))
    }

    fn parse_boolean(src: &str) -> Option<ParseResult<Json>> {
        JsonParser::parse_true(src).or(JsonParser::parse_false(src))
    }

    fn parse_string(src: &str) -> Option<ParseResult<Json>> {
        let (rest, string) = JsonParser::parse_string_literal(src)?;
        Some((rest, Json::String(string)))
    }

    fn parse_number(src: &str) -> Option<ParseResult<Json>> {
        match src.chars().nth(0)? {
            '+' => None,
            '-' => {
                if let Some((rest, Json::Number(n))) = JsonParser::parse_number(src.get(1..)?) {
                    Some((rest, Json::Number(-n)))
                } else {
                    None
                }
            }
            '0' => match src.chars().nth(1) {
                None => Some((src.get(1..)?, Json::Number(0.0))),
                Some(n) if n.is_ascii_digit() => None,
                _ => {
                    let (rest, chars) =
                        JsonParser::parse_chars_while(src, |c| c.is_ascii_digit() || c == '.')?;
                    Some((rest, Json::Number(chars.parse().ok()?)))
                }
            },
            _ => {
                let (rest, chars) =
                    JsonParser::parse_chars_while(src, |c| c.is_ascii_digit() || c == '.')?;
                Some((rest, Json::Number(chars.parse().ok()?)))
            }
        }
    }

    fn parse_array(src: &str) -> Option<ParseResult<Json>> {
        let mut vec = vec![];

        let (mut rest, _) = JsonParser::parse_char(src, '[')?;
        (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
        if rest.chars().nth(0)? == ']' {
            let rest = rest.get(1..)?;
            return Some((rest, Json::Array(vec)));
        }

        loop {
            let (after_item, item) = JsonParser::partial_parse(rest)?;
            rest = after_item;
            vec.push(item);
            (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
            let (after_c, c) = JsonParser::parse_char_if(rest, |c| c == ',' || c == ']')?;
            rest = after_c;
            match c {
                ',' => {
                    (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
                    continue;
                }
                ']' => break,
                _ => return None,
            }
        }

        Some((rest, Json::Array(vec)))
    }

    fn parse_object(src: &str) -> Option<ParseResult<Json>> {
        let mut object = HashMap::<String, Json>::new();

        let (mut rest, _) = JsonParser::parse_char(src, '{')?;
        (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
        if rest.chars().nth(0)? == '}' {
            let rest = rest.get(1..)?;
            return Some((rest, Json::Object(object)));
        }

        loop {
            let (after_key, key) = JsonParser::parse_string_literal(rest)?;
            rest = after_key;

            (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
            (rest, _) = JsonParser::parse_char(rest, ':')?;
            (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;

            let (after_value, value) = JsonParser::partial_parse(rest)?;
            rest = after_value;

            object.insert(key, value);

            (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
            let (after_line, c) = JsonParser::parse_char_if(rest, |c| c == ',' || c == '}')?;
            rest = after_line;
            match c {
                ',' => {
                    (rest, _) = JsonParser::parse_chars_while(rest, |c| c.is_whitespace())?;
                    continue;
                }
                '}' => break,
                _ => return None,
            }
        }

        Some((rest, Json::Object(object)))
    }

    fn partial_parse(src: &str) -> Option<ParseResult<Json>> {
        JsonParser::parse_null(src)
            .or_else(|| JsonParser::parse_boolean(src))
            .or_else(|| JsonParser::parse_string(src))
            .or_else(|| JsonParser::parse_number(src))
            .or_else(|| JsonParser::parse_array(src))
            .or_else(|| JsonParser::parse_object(src))
    }

    pub fn parse(src: &str) -> Option<Json> {
        JsonParser::partial_parse(src.trim())
            .and_then(|(rest, json)| (rest.trim().is_empty()).then(|| json))
    }
}

impl Json {
    pub fn null(&self) -> Option<()> {
        match self {
            Json::Null => Some(()),
            _ => None,
        }
    }

    pub fn bool(&self) -> Option<&bool> {
        match self {
            Json::Bool(boolean) => Some(boolean),
            _ => None,
        }
    }

    pub fn string(&self) -> Option<&String> {
        match self {
            Json::String(string) => Some(string),
            _ => None,
        }
    }

    pub fn number(&self) -> Option<&f64> {
        match self {
            Json::Number(number) => Some(number),
            _ => None,
        }
    }

    pub fn array(&self) -> Option<&JsonArray> {
        match self {
            Json::Array(array) => Some(array),
            _ => None,
        }
    }

    pub fn object(&self) -> Option<&JsonObject> {
        match self {
            Json::Object(object) => Some(object),
            _ => None,
        }
    }

    fn to_custom_string(&self) -> String {
        self.to_custom_string_with_tabs(0, "  ")
    }

    fn to_custom_string_with_tabs(&self, tabs: usize, tab_str: &str) -> String {
        format!(
            "{}",
            match self {
                Json::Null => "null".to_string(),
                Json::Bool(boolean) => boolean.to_string(),
                Json::String(string) => format!("\"{}\"", string),
                Json::Number(number) => number.to_string(),
                Json::Array(array) => {
                    if array.is_empty() {
                        "[]".to_string()
                    } else {
                        let string = array
                            .iter()
                            .map(|e| {
                                format!(
                                    "{}{}",
                                    tab_str.repeat(tabs + 1),
                                    e.to_custom_string_with_tabs(tabs + 1, tab_str)
                                )
                            })
                            .collect::<Vec<String>>()
                            .join(",\n");

                        format!("[\n{}\n{}]", string, tab_str.repeat(tabs))
                    }
                }
                Json::Object(object) => {
                    if object.is_empty() {
                        "{}".to_string()
                    } else {
                        let string = object
                            .iter()
                            .map(|(k, v)| {
                                format!(
                                    "{}\"{}\": {}",
                                    tab_str.repeat(tabs + 1),
                                    k,
                                    v.to_custom_string_with_tabs(tabs + 1, tab_str)
                                )
                            })
                            .collect::<Vec<String>>()
                            .join(",\n");
                        format!("{{\n{}\n{}}}", string, tab_str.repeat(tabs))
                    }
                }
            }
        )
    }
}

impl Display for Json {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_custom_string())
    }
}
