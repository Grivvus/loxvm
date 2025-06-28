const std = @import("std");
const scanner = @import("scanner.zig");
const Chunk = @import("bytecode.zig").Chunk;
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const Parser = struct {
    prev: Token,
    current: Token,
    hadError: bool,
    scanner: Scanner,

    pub fn init(sc: Scanner) Parser {
        return Parser{ .prev = undefined, .current = undefined, .hadError = false, .scanner = sc };
    }

    pub fn advance(self: *Parser) void {
        self.prev = self.current;
        while (true) {
            self.current = self.scanner.scanToken();
            if (self.current.type_ != .TOKEN_ERROR) {
                break;
            }
            errorAt(self.current, self.current.lexeme);
        }
    }

    pub fn consume(self: *Parser, type_: TokenType, msg: []const u8) void {
        if (self.current.type_ == type_) {
            self.advance();
        } else {
            errorAt(self.current, msg);
        }
    }
};

pub fn compile(source: []const u8, chunk: *Chunk, allocator: std.mem.Allocator) !void {
    const sc = try scanner.Scanner.init(source, allocator);
    var parser = Parser.init(sc);
}

pub fn errorAt(tok: Token, msg: []const u8) noreturn {
    std.debug.print("[line {d}] Error", .{tok.line});
    if (tok.type_ == .TOKEN_EOF) {
        std.debug.print(" at end", .{});
    } else if (tok.type_ != .TOKEN_ERROR) {
        std.debug.print(" at {s}", .{tok.lexeme});
    }
    std.debug.print(": {s}\n", .{msg});
    @panic("");
}
