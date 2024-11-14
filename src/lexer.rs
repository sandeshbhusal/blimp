use indexmap::IndexMap;
use regex::Regex;
use std::sync::LazyLock;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(EnumIter, Debug, Hash, Copy, Clone, PartialEq, Eq)]
pub(crate) enum TokenType {
    Lparen,
    Rparen,
    Lbrace,
    Rbrace,
    Colon,
    SemiColon,
    Comma,
    Arrow,

    KwLet,
    KwReturn,
    KwFalse,
    KwTrue,
    KwWhile,
    KwIf,
    KwElse,

    Plus,
    Minus,
    Multiply,
    Divide,
    Mod,

    Lt,
    Gt,
    Le,
    Ge,
    Ne,
    Eq,
    And,
    Or,
    Not,
    Assign,

    Identifier,

    Number,
    Dot,
}

static PATTERN_MAP: LazyLock<IndexMap<&'static str, TokenType>> = LazyLock::new(|| {
    let mut map = IndexMap::new();
    for variant in TokenType::iter() {
        let pattern = match variant {
            TokenType::KwLet => "let",
            TokenType::KwIf => "if",
            TokenType::KwElse => "else",
            TokenType::KwReturn => "return",
            TokenType::KwWhile => "while",
            TokenType::Lparen => r"\(",
            TokenType::Rparen => r"\)",
            TokenType::Lbrace => r"\{",
            TokenType::Rbrace => r"\}",
            TokenType::Colon => ":",
            TokenType::Comma => ",",
            TokenType::Number => r"\d+",
            TokenType::Identifier => r"[a-zA-Z_][a-zA-Z_0-9]*",
            TokenType::Plus => r"\+",
            TokenType::Minus => "-",
            TokenType::Le => "<=",
            TokenType::Ge => ">=",
            TokenType::Lt => "<",
            TokenType::Gt => ">",
            TokenType::Eq => "==",
            TokenType::Ne => "!=",
            TokenType::KwFalse => "false",
            TokenType::KwTrue => "true",
            TokenType::Multiply => r"\*",
            TokenType::Divide => "/",
            TokenType::And => "and",
            TokenType::Or => "or",
            TokenType::Not => "not",
            TokenType::Dot => r"\.",
            TokenType::Mod => "%",
            TokenType::Assign => "=",
            TokenType::Arrow => "->",
            TokenType::SemiColon => ";",
        };

        map.insert(pattern, variant);
    }

    map
});


#[derive(Debug, Clone)]
pub(crate) struct Token {
    pub r#type: TokenType,
    pub start: usize,
    pub end: usize,
    pub content: String,
}

pub fn lexer(input: &str) -> Result<Vec<Token>, (Vec<Token>, String)> {
    let mut offset = 0;
    let mut tokens = Vec::new();

    while offset < input.len() {
        if let Some((off, _)) = input
            .chars()
            .enumerate()
            .find(|(off, ip)| *off >= offset && !ip.is_whitespace())
        {
            offset = off;
        } else {
            return Ok(tokens); // Nothing except whitespaces found. Return lexed this far.
        }

        let mut found = false;

        for (pattern_str, &token_type) in PATTERN_MAP.iter() {
            let pattern = Regex::new(pattern_str).expect("Invalid regex pattern");
            if let Some(cap) = pattern.find(&input[offset..]) {
                if cap.start() == 0 {
                    let start = offset;
                    let end = offset + cap.end();
                    let content = &input[start..end];

                    tokens.push(Token {
                        r#type: token_type,
                        start,
                        end,
                        content: content.to_string(),
                    });

                    offset = end;
                    found = true;
                    break;
                }
            }
        }

        if !found {
            return Err((tokens, (&input[offset..]).to_string()));
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod lexer_tests {
    use super::lexer;

    #[test]
    fn test_valid_program() {
        let program = r#"
        let gcd: fn(i32, i32): i32 = (x, y) {
            if y == 0 {
                return x;
            } else {
                let temp: int = y;
                y = y % x;
                x = temp;

                let add_simd: func(vec<i32>, vec<i32>):() = (x, y) {
                    let result: vec<i32> = simdadd(x, y);
                } 

                return gcd(x, y);
            }
        }
        "#;

        match lexer(program) {
            Ok(_tokens) => {
            }
            Err((tokens, remaining)) => {
                panic!("Error: {:?}, Remaining: {}", tokens, remaining);
            }
        }
    }
}
