const std = @import("std");

pub const TokenType = enum {
    // single-character tokens
    TOKEN_LEFT_PAREN,
    TOKEN_RIGHT_PAREN,
    TOKEN_LEFT_BRACE,
    TOKEN_RIGHT_BRACE,
    TOKEN_COMMA,
    TOKEN_DOT,
    TOKEN_MINUS,
    TOKEN_PLUS,
    TOKEN_SEMICOLON,
    TOKEN_SLASH,
    TOKEN_STAR,
    // one-or-two character tokens
    TOKEN_BANG,
    TOKEN_BANG_EQUAL,
    TOKEN_EQUAL,
    TOKEN_EQUAL_EQUAL,
    TOKEN_GREATER,
    TOKEN_GREATER_EQUAL,
    TOKEN_LESS,
    TOKEN_LESS_EQUAL,
    // literals
    TOKEN_IDENTIFIER,
    TOKEN_STRING,
    TOKEN_NUMBER,
    // keywords
    TOKEN_AND,
    TOKEN_CLASS,
    TOKEN_ELSE,
    TOKEN_FALSE,
    TOKEN_FOR,
    TOKEN_FUN,
    TOKEN_IF,
    TOKEN_NIL,
    TOKEN_OR,
    TOKEN_PRINT,
    TOKEN_RETURN,
    TOKEN_SUPER,
    TOKEN_THIS,
    TOKEN_TRUE,
    TOKEN_VAR,
    TOKEN_WHILE,
    // other
    TOKEN_ERROR,
    TOKEN_EOF,
};

pub const Token = struct {
    type_: TokenType,
    lexeme: []const u8,
    line: i32,
    fn init(type_: TokenType, lexeme: []const u8, line: i32) Token {
        return Token{
            .type_ = type_,
            .lexeme = lexeme,
            .line = line,
        };
    }
};

pub const Scanner = struct {
    source: []const u8,
    current_index: i32,
    current_line: i32,

    pub fn init(source: []const u8) Scanner {
        return Scanner{
            .source = source,
            .current_index = 0,
            .current_line = 1,
        };
    }
    pub fn deinit(self: *Scanner) void {
        _ = self;
    }

    pub fn scanToken(self: *Scanner) Token {
        if (self.isAtEnd()) {
            return Token.init(.TOKEN_EOF, "", self.current_line);
        }
        const c = self.advance();
        if (isDigit(c)) {
            return self.number();
        }
        return switch (c) {
            '(' => return Token.init(.TOKEN_LEFT_PAREN, "(", self.current_line),
            ')' => return Token.init(.TOKEN_RIGHT_PAREN, ")", self.current_line),
            '{' => return Token.init(.TOKEN_LEFT_BRACE, "{", self.current_line),
            '}' => return Token.init(.TOKEN_RIGHT_BRACE, "}", self.current_line),
            ';' => return Token.init(.TOKEN_SEMICOLON, ";", self.current_line),
            ',' => return Token.init(.TOKEN_COMMA, ",", self.current_line),
            '.' => return Token.init(.TOKEN_DOT, ".", self.current_line),
            '-' => return Token.init(.TOKEN_MINUS, "-", self.current_line),
            '+' => return Token.init(.TOKEN_PLUS, "+", self.current_line),
            '/' => return Token.init(.TOKEN_SLASH, "/", self.current_line),
            '*' => return Token.init(.TOKEN_STAR, "*", self.current_line),
            '!' => return {
                if (self.match('=')) {
                    return Token.init(.TOKEN_BANG_EQUAL, "!=", self.current_line);
                } else {
                    return Token.init(.TOKEN_BANG, "!", self.current_line);
                }
            },
            '=' => return {
                if (self.match('=')) {
                    return Token.init(.TOKEN_EQUAL_EQUAL, "==", self.current_line);
                } else {
                    return Token.init(.TOKEN_EQUAL, "=", self.current_line);
                }
            },
            '<' => return {
                if (self.match('=')) {
                    return Token.init(.TOKEN_LESS_EQUAL, "<=", self.current_line);
                } else {
                    return Token.init(.TOKEN_LESS, "<", self.current_line);
                }
            },
            '>' => return {
                if (self.match('=')) {
                    return Token.init(.TOKEN_GREATER_EQUAL, ">=", self.current_line);
                } else {
                    return Token.init(.TOKEN_GREATER, ">", self.current_line);
                }
            },
            '"' => return self.string(),
            else => return Token.init(.TOKEN_ERROR, "Unexpected character", self.current_line),
        };
    }

    fn string(self: *Scanner) Token {
        var buffer: [10_000]u8 = undefined;
        var i = 0;
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.current_line += 1;
            }
            buffer[i] = self.advance();
            i += 1;
        }
        if (self.isAtEnd()) {
            return Token.init(.TOKEN_ERROR, "Unterminated string", self.current_line);
        }
        return Token.init(.TOKEN_STRING, buffer[0..i], self.current_line);
    }

    fn number(self: *Scanner) Token {
        var buffer: [20]u8 = undefined;
        var i = 0;
        while (isDigit(self.peek())) {
            buffer[i] = self.advance();
            i += 1;
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            buffer[i] = self.advance();
            i += 1;
            while (isDigit(self.peek)) {
                buffer[i] = self.advance();
                i += 1;
            }
        }
        return Token.init(.TOKEN_NUMBER, buffer[0..i], self.current_line);
    }

    fn isAtEnd(self: *Scanner) bool {
        return self.current_index == (self.source.len - 1);
    }

    fn advance(self: *Scanner) u8 {
        self.current_index += 1;
        return self.source[self.current_index - 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) {
            return false;
        }
        if (self.source[self.current_index] != expected) {
            return false;
        }
        self.current_index += 1;
        return true;
    }
    fn skipWhitespace(self: *Scanner) void {
        while (true) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    self.advance();
                },
                '\n' => {
                    self.current_line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            self.advance();
                        }
                    }
                },
                else => return,
            }
        }
    }
    fn peek(self: *Scanner) u8 {
        return self.source[self.current_index];
    }
    fn peekNext(self: *Scanner) u8 {
        if (self.isAtEnd()) {
            return 0;
        }
        return self.source[self.current_index + 1];
    }
};

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}
