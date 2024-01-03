use std::process::exit;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Exit,
    IntLiteral,
    Semi,
    OpenParent,
    CloseParent,
    Ident,
    Let,
    Eq,
    Plus,
    Star,
    Minus,
    Slash,
    OpenCurly,
    CloseCurly,
    If,
    Int,
    Bool,
    Colon,
    True,
    False,
    And,
    Or,
    Print,
    Println,
    Char,
    CharLiteral,
    LessThan,
    GreaterThan,
    Exclamation,
    While,
    Else,
}

pub fn bin_precedence(token_type: &TokenType) -> Option<i32> {
    match token_type {
        TokenType::Plus => Some(0),
        TokenType::Minus => Some(0),
        TokenType::Star => Some(1),
        TokenType::Slash => Some(1),
        TokenType::And => Some(0),
        TokenType::Or => Some(0),
        TokenType::LessThan => Some(0),
        TokenType::GreaterThan => Some(0),
        TokenType::Eq => Some(0),
        TokenType::Exclamation => Some(0),
        _ => None,
    }
}



#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub _type: TokenType,
    pub value: Option<String>,
}

pub struct Tokenizer {
    m_src: Vec<char>,
    tokens: Vec<Token>,
    index: usize,
}

impl Tokenizer {
    pub fn new(src: String) -> Tokenizer {
        Tokenizer {
            m_src: src.chars().collect(),
            tokens: Vec::new(),
            index: 0,
        }
    }

    pub fn get_tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn tokenize(&mut self) {
        let mut buffer = String::new();

        while let Some(c) = self.peek(0) {
            if (c.is_alphabetic() || c == '_') && !c.is_numeric() {
                buffer.push(self.consume(1));

                while let Some(c) = self.peek(0) {
                    if !c.is_alphanumeric() && c != '_'{
                        break;
                    }
                    buffer.push(self.consume(1));
                }
                if buffer == "exit" {
                    self.tokens.push(Token {
                        _type: TokenType::Exit,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "let" {
                    self.tokens.push(Token {
                        _type: TokenType::Let,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "if" {
                    self.tokens.push(Token {
                        _type: TokenType::If,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "int" {
                    self.tokens.push(Token {
                        _type: TokenType::Int,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "bool" {
                    self.tokens.push(Token {
                        _type: TokenType::Bool,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "char" {
                    self.tokens.push(Token {
                        _type: TokenType::Char,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "true" {
                    self.tokens.push(Token {
                        _type: TokenType::True,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "false" {
                    self.tokens.push(Token {
                        _type: TokenType::False,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "println" {
                    self.tokens.push(Token {
                        _type: TokenType::Println,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "print" {
                    self.tokens.push(Token {
                        _type: TokenType::Print,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } else if buffer == "while" {
                    self.tokens.push(Token {
                        _type: TokenType::While,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                } 
                else if buffer == "else" {
                    self.tokens.push(Token {
                        _type: TokenType::Else,
                        value: None,
                    });
                    buffer.clear();
                    continue;
                }
                else if c != '_' {
                    self.tokens.push(Token {
                        _type: TokenType::Ident,
                        value: Some(buffer.clone()),
                    });
                    buffer.clear();
                    continue;
                }
               
            } else if c.is_numeric() {
                buffer.push(self.consume(1));
                while let Some(c) = self.peek(0) {
                    if !c.is_numeric() {
                        break;
                    }
                    buffer.push(self.consume(1));
                }
                self.tokens.push(Token {
                    _type: TokenType::IntLiteral,
                    value: Some(buffer.clone()),
                });
                buffer.clear();
                continue;
            } else if c == '(' {
                self.tokens.push(Token {
                    _type: TokenType::OpenParent,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == ')' {
                self.tokens.push(Token {
                    _type: TokenType::CloseParent,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == ';' {
                self.tokens.push(Token {
                    _type: TokenType::Semi,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == ':' {
                self.tokens.push(Token {
                    _type: TokenType::Colon,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '=' {
                self.tokens.push(Token {
                    _type: TokenType::Eq,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '+' {
                self.tokens.push(Token {
                    _type: TokenType::Plus,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '-' {
                self.tokens.push(Token {
                    _type: TokenType::Minus,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '/' {
                self.tokens.push(Token {
                    _type: TokenType::Slash,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '*' {
                self.tokens.push(Token {
                    _type: TokenType::Star,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == ' ' || c == '\n' {
                self.consume(1);
                continue;
            } else if c == '{' {
                self.tokens.push(Token {
                    _type: TokenType::OpenCurly,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '}' {
                self.tokens.push(Token {
                    _type: TokenType::CloseCurly,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '&' {
                self.consume(1);
                if let Some(c) = self.peek(0) {
                    if c == '&' {
                        self.tokens.push(Token {
                            _type: TokenType::And,
                            value: None,
                        });
                        self.consume(1);
                        continue;
                    }
                    eprintln!("Unknown token: {}", c);
                    exit(-1);
                }
                eprintln!("Expected token: {}", c);
                exit(-1);
            } else if c == '|' {
                self.consume(1);

                if let Some(c) = self.peek(0) {
                    if c == '|' {
                        self.tokens.push(Token {
                            _type: TokenType::Or,
                            value: None,
                        });
                        self.consume(1);
                        continue;
                    }
                    eprintln!("Unknown token: {}", c);
                    exit(-1);
                }
                eprintln!("Expected token: {}", c);
                exit(-1);
            } else if c == '\'' {
                self.consume(1);
                if let Some(c) = self.peek(0) {
                    self.tokens.push(Token {
                        _type: TokenType::CharLiteral,
                        value: Some(c.to_string()),
                    });
                    self.consume(1);
                }
                if let Some(c) = self.peek(0) {
                    if c != '\'' {
                        eprintln!("Expected token: {}", c);
                        exit(-1);
                    }
                }
                self.consume(1);
            } else if c == '<' {
                self.tokens.push(Token {
                    _type: TokenType::LessThan,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '>' {
                self.tokens.push(Token {
                    _type: TokenType::GreaterThan,
                    value: None,
                });
                self.consume(1);
                continue;
            } else if c == '!' {
                self.tokens.push(Token {
                    _type: TokenType::Exclamation,
                    value: None,
                });
                self.consume(1);
                continue;
            } else {
                eprintln!("Unknown token: {}", c);
                exit(-1);
            }
        }
        self.index = 0;
    }

    fn peek(&self, ahead: usize) -> Option<char> {
        if self.index + ahead >= self.m_src.len() {
            None
        } else {
            Some(self.m_src[self.index + ahead])
        }
    }

    fn consume(&mut self, ahead: usize) -> char {
        self.index += ahead;
        self.m_src[self.index - 1]
    }
}
