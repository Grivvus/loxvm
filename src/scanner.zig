const std = @import("std");
const Map = std.StringHashMap;
var arena = @import("main.zig").arena;

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

fn populateMap(keywords: *Map(TokenType)) !void {
    try keywords.put("and", .TOKEN_AND);
    try keywords.put("class", .TOKEN_CLASS);
    try keywords.put("else", .TOKEN_ELSE);
    try keywords.put("false", .TOKEN_FALSE);
    try keywords.put("for", .TOKEN_FOR);
    try keywords.put("fun", .TOKEN_FUN);
    try keywords.put("if", .TOKEN_IF);
    try keywords.put("nil", .TOKEN_NIL);
    try keywords.put("or", .TOKEN_OR);
    try keywords.put("print", .TOKEN_PRINT);
    try keywords.put("return", .TOKEN_RETURN);
    try keywords.put("super", .TOKEN_SUPER);
    try keywords.put("this", .TOKEN_THIS);
    try keywords.put("true", .TOKEN_TRUE);
    try keywords.put("var", .TOKEN_VAR);
    try keywords.put("while", .TOKEN_WHILE);
}

pub const Token = struct {
    type_: TokenType,
    lexeme: []const u8,
    line: usize,
    fn init(type_: TokenType, lexeme: []const u8, line: usize) Token {
        return Token{
            .type_ = type_,
            .lexeme = lexeme,
            .line = line,
        };
    }
};

pub const Scanner = struct {
    source: []const u8,
    current_index: usize,
    current_line: usize,
    keywords: Map(TokenType),

    pub fn init(source: []const u8, allocator: std.mem.Allocator) !Scanner {
        var keywords = Map(TokenType).init(allocator);
        try populateMap(&keywords);
        return Scanner{
            .source = source,
            .current_index = 0,
            .current_line = 1,
            .keywords = keywords,
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
        const start = self.current_index;
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.current_line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return Token.init(.TOKEN_ERROR, "Unterminated string", self.current_line);
        }
        const end = self.current_index;
        return Token.init(.TOKEN_STRING, self.source[start .. end + 1], self.current_line);
    }

    fn number(self: *Scanner) Token {
        const start = self.current_index;
        while (isDigit(self.peek())) {
            _ = self.advance();
        }
        if (self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        const end = self.current_index;
        return Token.init(.TOKEN_NUMBER, self.source[start .. end + 1], self.current_line);
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

    fn identifier(self: *Scanner) Token {
        const start = self.current_index;
        while (isAlpha(self.peek()) or isDigit(self.peek())) {
            _ = advance(self);
        }
        const end = self.current_index;
        const token_type = self.keywords.get(self.source[start .. end + 1]) orelse TokenType.TOKEN_IDENTIFIER;
        return Token.init(token_type, self.source[start .. end + 1]);
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

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}
