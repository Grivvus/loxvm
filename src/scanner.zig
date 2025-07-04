const std = @import("std");
var arena = @import("main.zig").arena;

pub const TokenType = enum {
    // single-character tokens
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SEMICOLON,
    SLASH,
    STAR,
    // one-or-two character tokens
    BANG,
    BANG_EQUAL,
    EQUAL,
    EQUAL_EQUAL,
    GREATER,
    GREATER_EQUAL,
    LESS,
    LESS_EQUAL,
    // literals
    IDENTIFIER,
    STRING,
    NUMBER,
    // keywords
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,
    // other
    ERROR,
    EOF,
};

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

const keywords = std.StaticStringMap(TokenType).initComptime(.{
    .{ "and", .AND },
    .{ "class", .CLASS },
    .{ "else", .ELSE },
    .{ "false", .FALSE },
    .{ "for", .FOR },
    .{ "fun", .FUN },
    .{ "if", .IF },
    .{ "nil", .NIL },
    .{ "or", .OR },
    .{ "print", .PRINT },
    .{ "return", .RETURN },
    .{ "super", .SUPER },
    .{ "this", .THIS },
    .{ "true", .THIS },
    .{ "var", .VAR },
    .{ "while", .WHILE },
});

pub const Scanner = struct {
    source: []const u8,
    current_index: usize,
    current_line: usize,
    allocator: std.mem.Allocator,
    keywords: std.StaticStringMap(TokenType),

    pub fn init(source: []const u8, allocator: std.mem.Allocator) !Scanner {
        return Scanner{
            .source = source,
            .current_index = 0,
            .current_line = 1,
            .allocator = allocator,
            .keywords = keywords,
        };
    }
    pub fn deinit(self: *Scanner) void {
        _ = self;
    }

    pub fn scanToken(self: *Scanner) Token {
        self.skipWhitespace();
        if (self.isAtEnd()) {
            return Token.init(.EOF, "", self.current_line);
        }
        const c = self.advance();
        if (isDigit(c)) {
            return self.number();
        }
        return switch (c) {
            '(' => return Token.init(.LEFT_PAREN, "(", self.current_line),
            ')' => return Token.init(.RIGHT_PAREN, ")", self.current_line),
            '{' => return Token.init(.LEFT_BRACE, "{", self.current_line),
            '}' => return Token.init(.RIGHT_BRACE, "}", self.current_line),
            ';' => return Token.init(.SEMICOLON, ";", self.current_line),
            ',' => return Token.init(.COMMA, ",", self.current_line),
            '.' => return Token.init(.DOT, ".", self.current_line),
            '-' => return Token.init(.MINUS, "-", self.current_line),
            '+' => return Token.init(.PLUS, "+", self.current_line),
            '/' => return Token.init(.SLASH, "/", self.current_line),
            '*' => return Token.init(.STAR, "*", self.current_line),
            '!' => return {
                if (self.match('=')) {
                    return Token.init(.BANG_EQUAL, "!=", self.current_line);
                } else {
                    return Token.init(.BANG, "!", self.current_line);
                }
            },
            '=' => return {
                if (self.match('=')) {
                    return Token.init(.EQUAL_EQUAL, "==", self.current_line);
                } else {
                    return Token.init(.EQUAL, "=", self.current_line);
                }
            },
            '<' => return {
                if (self.match('=')) {
                    return Token.init(.LESS_EQUAL, "<=", self.current_line);
                } else {
                    return Token.init(.LESS, "<", self.current_line);
                }
            },
            '>' => return {
                if (self.match('=')) {
                    return Token.init(.GREATER_EQUAL, ">=", self.current_line);
                } else {
                    return Token.init(.GREATER, ">", self.current_line);
                }
            },
            '"' => return self.string(),
            else => return Token.init(.ERROR, std.fmt.allocPrint(self.allocator, "Unexpected character '{c}'", .{self.source[self.current_index - 1]}) catch unreachable, self.current_line),
        };
    }

    fn string(self: *Scanner) Token {
        const start = self.current_index - 1;
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') {
                self.current_line += 1;
            }
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            return Token.init(.ERROR, "Unterminated string", self.current_line);
        }
        const end = self.current_index;
        return Token.init(.STRING, self.source[start..end], self.current_line);
    }

    fn number(self: *Scanner) Token {
        const start = self.current_index - 1;
        while (!self.isAtEnd() and isDigit(self.peek())) {
            _ = self.advance();
        }
        if (!self.isAtEnd() and self.peek() == '.' and isDigit(self.peekNext())) {
            _ = self.advance();
            while (!self.isAtEnd() and isDigit(self.peek())) {
                _ = self.advance();
            }
        }
        const end = self.current_index;
        return Token.init(.NUMBER, self.source[start..end], self.current_line);
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
        while (!self.isAtEnd()) {
            const c = self.peek();
            switch (c) {
                ' ', '\r', '\t' => {
                    _ = self.advance();
                },
                '\n' => {
                    self.current_line += 1;
                    _ = self.advance();
                },
                '/' => {
                    if (self.peekNext() == '/') {
                        while (self.peek() != '\n' and !self.isAtEnd()) {
                            _ = self.advance();
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
        const token_type = self.keywords.get(self.source[start .. end + 1]) orelse TokenType.IDENTIFIER;
        return Token.init(token_type, self.source[start .. end + 1]);
    }

    fn peekPrev(self: *Scanner) u8 {
        return self.source[self.current_index - 1];
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
