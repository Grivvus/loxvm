const std = @import("std");
const scanner = @import("scanner.zig");
const Chunk = @import("bytecode.zig").Chunk;
const OpCode = @import("bytecode.zig").OpCode;
const Value = @import("value.zig").Value;
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

var compiling_chunk: *Chunk = undefined;
pub fn compile(source: []const u8, chunk: *Chunk, allocator: std.mem.Allocator) !void {
    const sc = try scanner.Scanner.init(source, allocator);
    var parser = Parser.init(sc);
    compiling_chunk = chunk;

    try endCompiler();
}

fn expression(parser: Parser) !void {}

fn number(parser: Parser) !void {
    const parsed_value = try std.fmt.parseFloat(f64, parser.prev.lexeme);
    emitConstant(parsed_value, parser);
}

fn unary(parser: Parser) !void {
    const operator_type = parser.prev.type_;
    expression(parser);
    switch (operator_type) {
        .TOKEN_MINUS => try emitOpcode(OpCode.OP_NEGATE),
        else => unreachable,
    }
}

fn grouping(parser: Parser) !void {
    try expression(parser);
    parser.consume(.TOKEN_RIGHT_PAREN, "expect ')' after expression");
}

fn endCompiler(parser: Parser) !void {
    try emitOpcode(@enumFromInt(.OP_RETURN), parser);
}

fn emitOpcode(opcode: u8, parser: Parser) !void {
    try currentChunk().write(opcode, parser.prev.line);
}

fn emitOpcodes(op1: u8, op2: u8, parser: Parser) !void {
    try emitOpcode(op1, parser);
    try emitOpcode(op2, parser);
}

fn emitConstant(value: Value, parser: Parser) !void {
    try emitOpcodes(@enumFromInt(.OP_CONSTANT), makeConstant(value), parser);
}

fn makeConstant(value: Value) u8 {
    const index: u8 = @intCast(currentChunk().addConstant(value));
    return index;
}

fn currentChunk() *Chunk {
    if (compiling_chunk == undefined) {
        @panic("compiling_chunk never assigned");
    }
    return compiling_chunk;
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
