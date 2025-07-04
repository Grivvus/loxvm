const std = @import("std");
const builtin = @import("builtin");
const scanner = @import("scanner.zig");
const debug = @import("debug.zig");
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
            if (self.current.type_ != .ERROR) {
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

const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // ==, !=
    COMPARISON, // <, >, <=, >=
    TERM, // +, -
    FACTOR, // *, /
    UNARY, // !, -
    CALL, // ., ()
    PRIMARY,
};

const ParseRule = struct {
    prefix: ?*const fn (parser: *Parser) anyerror!void,
    infix: ?*const fn (parser: *Parser) anyerror!void,
    precedence: Precedence,
};

var rules: [256]ParseRule = undefined;
fn fillUpRules() void {
    const tt = TokenType;
    rules[@intFromEnum(tt.LEFT_PAREN)] = ParseRule{
        .prefix = grouping,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.RIGHT_PAREN)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.LEFT_BRACE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.RIGHT_BRACE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.COMMA)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.DOT)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.MINUS)] = ParseRule{
        .prefix = unary,
        .infix = binary,
        .precedence = .TERM,
    };
    rules[@intFromEnum(tt.PLUS)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .TERM,
    };
    rules[@intFromEnum(tt.SEMICOLON)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.SLASH)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .FACTOR,
    };
    rules[@intFromEnum(tt.STAR)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .FACTOR,
    };
    rules[@intFromEnum(tt.BANG)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.BANG_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.EQUAL_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.GREATER)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.GREATER_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.LESS)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.LESS_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.IDENTIFIER)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.STRING)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.NUMBER)] = ParseRule{
        .prefix = number,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.AND)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.CLASS)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.ELSE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.FALSE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.FOR)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.FUN)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.IF)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.NIL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.OR)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.PRINT)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.RETURN)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.SUPER)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.THIS)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.TRUE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.VAR)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.WHILE)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.ERROR)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.EOF)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
}

fn getRule(tt: TokenType) *ParseRule {
    return &rules[@intFromEnum(tt)];
}

var compiling_chunk: *Chunk = undefined;
pub fn compile(source: []const u8, chunk: *Chunk, allocator: std.mem.Allocator) !void {
    fillUpRules();
    const sc = try scanner.Scanner.init(source, allocator);
    var parser = Parser.init(sc);
    compiling_chunk = chunk;
    parser.advance();
    try expression(&parser);

    parser.consume(.EOF, "expect end of expression");
    try endCompiler(&parser);
}

fn expression(parser: *Parser) !void {
    try parsePrecedence(.ASSIGNMENT, parser);
}

fn number(parser: *Parser) !void {
    const parsed_value = try std.fmt.parseFloat(f64, parser.prev.lexeme);
    try emitConstant(parsed_value, parser);
}

fn unary(parser: *Parser) !void {
    const operator_type = parser.prev.type_;

    try parsePrecedence(.UNARY, parser);

    switch (operator_type) {
        .MINUS => try emitOpcode(@intFromEnum(OpCode.OP_NEGATE), parser),
        else => unreachable,
    }
}

fn binary(parser: *Parser) !void {
    const operator_type = parser.prev.type_;
    const rule: *ParseRule = getRule(operator_type);
    try parsePrecedence(rule.precedence, parser);

    switch (operator_type) {
        .PLUS => try emitOpcode(@intFromEnum(OpCode.OP_ADD), parser),
        .MINUS => try emitOpcode(@intFromEnum(OpCode.OP_SUBSTRUCT), parser),
        .STAR => try emitOpcode(@intFromEnum(OpCode.OP_MULTIPLY), parser),
        .SLASH => try emitOpcode(@intFromEnum(OpCode.OP_DIVIDE), parser),
        else => unreachable,
    }
}

fn parsePrecedence(precedence: Precedence, parser: *Parser) !void {
    parser.advance();
    const prefixRule = getRule(parser.prev.type_).prefix;
    if (prefixRule == null) {
        errorAt(parser.prev, "Expect expression");
        return;
    }
    try prefixRule.?(parser);

    while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type_).precedence)) {
        parser.advance();
        const infixRule = getRule(parser.prev.type_).infix;
        try infixRule.?(parser);
    }
}

fn grouping(parser: *Parser) !void {
    try expression(parser);
    parser.consume(.RIGHT_PAREN, "expect ')' after expression");
}

fn endCompiler(parser: *Parser) !void {
    try emitOpcode(@intFromEnum(OpCode.OP_RETURN), parser);
    if (builtin.mode == .Debug) {
        if (parser.hadError == false) {
            debug.disassembleChunk(currentChunk().*, "code");
        }
    }
}

fn emitOpcode(opcode: u8, parser: *Parser) !void {
    try currentChunk().write(opcode, parser.prev.line);
}

fn emitOpcodes(op1: u8, op2: u8, parser: *Parser) !void {
    try emitOpcode(op1, parser);
    try emitOpcode(op2, parser);
}

fn emitConstant(value: Value, parser: *Parser) !void {
    try emitOpcodes(@intFromEnum(OpCode.OP_CONSTANT), try makeConstant(value), parser);
}

fn makeConstant(value: Value) !u8 {
    const index: u8 = @intCast(try currentChunk().addConstant(value));
    return index;
}

fn currentChunk() *Chunk {
    // i hope at this moment compiling_chunk is not undefined
    return compiling_chunk;
}

pub fn errorAt(tok: Token, msg: []const u8) noreturn {
    std.debug.print("[line {d}] Error", .{tok.line});
    if (tok.type_ == .EOF) {
        std.debug.print(" at end", .{});
    } else if (tok.type_ != .ERROR) {
        std.debug.print(" at {s}", .{tok.lexeme});
    }
    std.debug.print(": {s}\n", .{msg});
    @panic("");
}
