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
    fn parse_normal_char(src: &str) -> Option<ParseResult<char>> {
        let c = src.chars().nth(0)?;
        (c != '\\').then(|| Some((src.get(1..)?, c)))?
    }

    fn parse_escaped_char(src: &str) -> Option<ParseResult<char>> {
        let c = src.chars().nth(0)?;
        if c != '\\' {
            return None;
        }
        let e = match src.chars().nth(1)? {
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            '\\' => '\\',
            '/' => '/',
            '"' => '"',
            _ => return None,
        };
        (c == '\\').then(|| Some((src.get(2..)?, e)))?
    }

    fn parse_char(src: &str, c: char) -> Option<ParseResult<char>> {
        JsonParser::parse_normal_char(src)
            .or_else(|| JsonParser::parse_escaped_char(src))
            .and_then(|(rest, ch)| (ch == c).then(|| (rest, c)))
    }

    fn parse_char_if<F: Fn(char) -> bool>(src: &str, f: F) -> Option<ParseResult<char>> {
        JsonParser::parse_normal_char(src)
            .or_else(|| JsonParser::parse_escaped_char(src))
            .and_then(|(rest, c)| f(c).then(|| (rest, c)))
    }

    fn parse_whitespace(src: &str) -> Option<ParseResult<()>> {
        let mut rest = src;

        for _ in src.chars() {
            let (str, c) = JsonParser::parse_normal_char(rest)
                .or_else(|| JsonParser::parse_escaped_char(rest))?;

            if !c.is_whitespace() {
                break;
            }

            rest = str;
        }

        Some((rest, ()))
    }

    fn parse_chars<'s>(src: &'s str, str: &str) -> Option<ParseResult<'s, String>> {
        if src.len() < str.len() {
            return None;
        }

        let mut chars = String::new();
        let mut rest = src;

        for ch in str.chars() {
            let (str, c) = JsonParser::parse_normal_char(rest)
                .or_else(|| JsonParser::parse_escaped_char(rest))?;
            if ch != c {
                return None;
            }
            chars.push(ch);
            rest = str;
        }

        Some((rest, chars))
    }

    fn parse_chars_while<F: Fn(char) -> bool>(src: &str, f: F) -> Option<ParseResult<String>> {
        let mut buffer = String::new();
        let mut rest = src;

        for _ in src.chars() {
            let mut escaped = false;
            let (str, c) = JsonParser::parse_normal_char(rest).or_else(|| {
                escaped = true;
                JsonParser::parse_escaped_char(rest)
            })?;

            if !escaped && !f(c) {
                break;
            }

            buffer.push(c);
            rest = str;
        }

        Some((rest, buffer))
    }

    fn parse_true(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "true")?;
        Some((rest, Json::Bool(true)))
    }

    fn parse_false(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "false")?;
        Some((rest, Json::Bool(false)))
    }

    fn parse_string_literal(src: &str) -> Option<ParseResult<String>> {
        let (rest, _) = JsonParser::parse_char(src, '\"')?;
        let (rest, string) = JsonParser::parse_chars_while(rest, |c| {
            c != '\"' && c != '\n' && c != '\r' && c != '\t'
        })?;
        let (rest, _) = JsonParser::parse_char(rest, '\"')?;
        Some((rest, String::from(string)))
    }

    fn parse_null(src: &str) -> Option<ParseResult<Json>> {
        let (rest, _) = JsonParser::parse_chars(src, "null")?;
        Some((rest, Json::Null))
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
        let (mut rest, _) = JsonParser::parse_char(src, '[')?;
        (rest, _) = JsonParser::parse_whitespace(rest)?;
        if rest.chars().nth(0)? == ']' {
            let rest = rest.get(1..)?;
            return Some((rest, Json::Array(vec![])));
        }

        let mut vec = vec![];

        loop {
            let (after_item, item) = JsonParser::partial_parse(rest)?;
            rest = after_item;
            vec.push(item);

            (rest, _) = JsonParser::parse_whitespace(rest)?;
            let (after_c, c) = JsonParser::parse_char_if(rest, |c| c == ',' || c == ']')?;
            rest = after_c;

            match c {
                ',' => {
                    (rest, _) = JsonParser::parse_whitespace(rest)?;
                    continue;
                }
                ']' => break,
                _ => return None,
            }
        }

        Some((rest, Json::Array(vec)))
    }

    fn parse_object(src: &str) -> Option<ParseResult<Json>> {
        let (mut rest, _) = JsonParser::parse_char(src, '{')?;
        (rest, _) = JsonParser::parse_whitespace(rest)?;
        if rest.chars().nth(0)? == '}' {
            let rest = rest.get(1..)?;
            return Some((rest, Json::Object(HashMap::new())));
        }

        let mut object = HashMap::new();

        loop {
            let (after_key, key) = JsonParser::parse_string_literal(rest)?;
            rest = after_key;

            (rest, _) = JsonParser::parse_whitespace(rest)?;
            (rest, _) = JsonParser::parse_char(rest, ':')?;
            (rest, _) = JsonParser::parse_whitespace(rest)?;

            let (after_value, value) = JsonParser::partial_parse(rest)?;
            rest = after_value;

            object.insert(key, value);

            (rest, _) = JsonParser::parse_whitespace(rest)?;
            let (after_line, c) = JsonParser::parse_char_if(rest, |c| c == ',' || c == '}')?;
            rest = after_line;

            match c {
                ',' => {
                    (rest, _) = JsonParser::parse_whitespace(rest)?;
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
            .and_then(|(rest, json)| (rest.is_empty()).then(|| json))
    }
}

pub trait JsonValue {
    fn null(&self) -> Option<()>;

    fn bool(&self) -> Option<&bool>;

    fn string(&self) -> Option<&String>;

    fn number(&self) -> Option<&f64>;

    fn array(&self) -> Option<&JsonArray>;

    fn object(&self) -> Option<&JsonObject>;
}

impl Json {
    fn to_custom_string(&self, tab: &str) -> String {
        self.to_custom_string_with_tabs(0, tab)
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

impl JsonValue for Json {
    fn null(&self) -> Option<()> {
        match self {
            Json::Null => Some(()),
            _ => None,
        }
    }

    fn bool(&self) -> Option<&bool> {
        match self {
            Json::Bool(boolean) => Some(boolean),
            _ => None,
        }
    }

    fn string(&self) -> Option<&String> {
        match self {
            Json::String(string) => Some(string),
            _ => None,
        }
    }

    fn number(&self) -> Option<&f64> {
        match self {
            Json::Number(number) => Some(number),
            _ => None,
        }
    }

    fn array(&self) -> Option<&JsonArray> {
        match self {
            Json::Array(array) => Some(array),
            _ => None,
        }
    }

    fn object(&self) -> Option<&JsonObject> {
        match self {
            Json::Object(object) => Some(object),
            _ => None,
        }
    }
}

impl JsonValue for Option<Json> {
    fn null(&self) -> Option<()> {
        match self {
            Some(Json::Null) => Some(()),
            _ => None,
        }
    }

    fn bool(&self) -> Option<&bool> {
        match self {
            Some(Json::Bool(boolean)) => Some(boolean),
            _ => None,
        }
    }

    fn string(&self) -> Option<&String> {
        match self {
            Some(Json::String(string)) => Some(string),
            _ => None,
        }
    }

    fn number(&self) -> Option<&f64> {
        match self {
            Some(Json::Number(number)) => Some(number),
            _ => None,
        }
    }

    fn array(&self) -> Option<&JsonArray> {
        match self {
            Some(Json::Array(array)) => Some(array),
            _ => None,
        }
    }

    fn object(&self) -> Option<&JsonObject> {
        match self {
            Some(Json::Object(object)) => Some(object),
            _ => None,
        }
    }
}

impl JsonValue for Option<&Json> {
    fn null(&self) -> Option<()> {
        match self {
            Some(Json::Null) => Some(()),
            _ => None,
        }
    }

    fn bool(&self) -> Option<&bool> {
        match self {
            Some(Json::Bool(boolean)) => Some(boolean),
            _ => None,
        }
    }

    fn string(&self) -> Option<&String> {
        match self {
            Some(Json::String(string)) => Some(string),
            _ => None,
        }
    }

    fn number(&self) -> Option<&f64> {
        match self {
            Some(Json::Number(number)) => Some(number),
            _ => None,
        }
    }

    fn array(&self) -> Option<&JsonArray> {
        match self {
            Some(Json::Array(array)) => Some(array),
            _ => None,
        }
    }

    fn object(&self) -> Option<&JsonObject> {
        match self {
            Some(Json::Object(object)) => Some(object),
            _ => None,
        }
    }
}

impl Display for Json {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_custom_string("  "))
    }
}
