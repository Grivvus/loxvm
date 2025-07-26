const std = @import("std");
const builtin = @import("builtin");
const scanner = @import("scanner.zig");
const object = @import("object.zig");
const debug = @import("debug.zig");
const Chunk = @import("bytecode.zig").Chunk;
const OpCode = @import("bytecode.zig").OpCode;
const Value = @import("value.zig").Value;
const Object = object.Object;
const ObjString = object.ObjString;
const ObjFunction = object.ObjFunction;
const Scanner = scanner.Scanner;
const Token = scanner.Token;
const TokenType = scanner.TokenType;

const Parser = struct {
    prev: Token,
    current: Token,
    hadError: bool,
    scanner: Scanner,
    compiler: *Compiler,
    object_allocator: std.mem.Allocator,

    pub fn init(sc: Scanner, compiler: *Compiler, alloc: std.mem.Allocator) Parser {
        return Parser{
            .prev = undefined,
            .current = undefined,
            .hadError = false,
            .scanner = sc,
            .object_allocator = alloc,
            .compiler = compiler,
        };
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
    pub fn match(self: *Parser, type_: TokenType) bool {
        if (self.current.type_ != type_) {
            return false;
        }
        self.advance();
        return true;
    }
    pub fn check(self: *Parser, type_: TokenType) bool {
        return self.current.type_ == type_;
    }
};

const Local = struct {
    name: Token,
    depth: i32,
};

const FunctionType = enum {
    FUNCTION,
    SCRIPT,
};

const CompilerError = error{
    CantResolve,
    CompilationError,
};

const Compiler = struct {
    enclosing: ?*Compiler,
    function: *ObjFunction,
    function_type: FunctionType,
    locals: [256]Local,
    local_cnt: usize,
    scope_depth: i32,
    alloc: std.mem.Allocator,
    pub fn init(
        alloc: std.mem.Allocator,
        function_type: FunctionType,
        function_name: ?*ObjString,
    ) !*Compiler {
        var compiler = try alloc.create(Compiler);
        compiler.enclosing = current_compiler;
        compiler.locals = undefined;
        compiler.local_cnt = 0;
        compiler.scope_depth = 0;
        compiler.function = try ObjFunction.init(alloc);
        compiler.function.name = function_name;
        compiler.function_type = function_type;
        compiler.alloc = alloc;

        current_compiler = compiler;

        var local = &current_compiler.?.locals[current_compiler.?.local_cnt];
        local.depth = 0;
        local.name = Token.init(.IDENTIFIER, "", 0);
        current_compiler.?.local_cnt += 1;

        return compiler;
    }
    pub fn deinit(self: *Compiler) void {
        self.function.deinit();
        self.alloc.destroy(self);
    }
    pub fn beginScope(self: *Compiler) void {
        self.scope_depth += 1;
    }
    pub fn endScope(self: *Compiler, parser: *Parser) !void {
        self.scope_depth -= 1;

        while (self.local_cnt > 0 and self.locals[self.local_cnt - 1].depth > self.scope_depth) {
            try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
            self.local_cnt -= 1;
        }
    }
    pub fn resolve(self: *Compiler, name: Token) CompilerError!usize {
        var i = self.local_cnt;
        while (i > 0) {
            i -= 1;
            const local = &self.locals[i];
            if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
                if (local.*.depth == -1) {
                    errorAt(name, "Can't read local variable in it's own initializer");
                }
                return i;
            }
        }
        return CompilerError.CantResolve;
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
    prefix: ?*const fn (parser: *Parser, can_assign: bool) anyerror!void,
    infix: ?*const fn (parser: *Parser, can_assign: bool) anyerror!void,
    precedence: Precedence,
};

var rules: [256]ParseRule = undefined;
fn fillUpRules() void {
    const tt = TokenType;
    rules[@intFromEnum(tt.LEFT_PAREN)] = ParseRule{
        .prefix = grouping,
        .infix = call,
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
        .prefix = unary,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.BANG_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .EQUALITY,
    };
    rules[@intFromEnum(tt.EQUAL)] = ParseRule{
        .prefix = null,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.EQUAL_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .EQUALITY,
    };
    rules[@intFromEnum(tt.GREATER)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .COMPARISON,
    };
    rules[@intFromEnum(tt.GREATER_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .COMPARISON,
    };
    rules[@intFromEnum(tt.LESS)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .COMPARISON,
    };
    rules[@intFromEnum(tt.LESS_EQUAL)] = ParseRule{
        .prefix = null,
        .infix = binary,
        .precedence = .COMPARISON,
    };
    rules[@intFromEnum(tt.IDENTIFIER)] = ParseRule{
        .prefix = variable,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.STRING)] = ParseRule{
        .prefix = string,
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
        .infix = and_,
        .precedence = .AND,
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
        .prefix = literal,
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
        .prefix = literal,
        .infix = null,
        .precedence = .NONE,
    };
    rules[@intFromEnum(tt.OR)] = ParseRule{
        .prefix = null,
        .infix = or_,
        .precedence = .OR,
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
        .prefix = literal,
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

var current_compiler: ?*Compiler = null;
pub fn compile(source: []const u8, arena: std.mem.Allocator, obj_alloc: std.mem.Allocator) !*ObjFunction {
    fillUpRules();
    const sc = try scanner.Scanner.init(source, arena);
    const compiler = try Compiler.init(arena, .SCRIPT, null);
    var parser = Parser.init(sc, compiler, obj_alloc);
    parser.advance();
    while (!parser.match(.EOF)) {
        try declaration(&parser);
    }

    parser.consume(.EOF, "expect end of expression");
    return endCompiler(&parser);
}

fn expression(parser: *Parser) !void {
    try parsePrecedence(.ASSIGNMENT, parser);
}

fn declaration(parser: *Parser) !void {
    if (parser.match(.FUN)) {
        try funDeclaration(parser);
    } else if (parser.match(.VAR)) {
        try varDeclaration(parser);
    } else {
        try statement(parser);
    }
}

fn varDeclaration(parser: *Parser) !void {
    const global = try parseVariable(parser, "Expect variable name");

    if (parser.match(.EQUAL)) {
        try expression(parser);
    } else {
        try emitOpcode(@intFromEnum(OpCode.OP_NIL), parser);
    }
    parser.consume(.SEMICOLON, "Expect ';' after variable declaration");
    try defineVariable(parser, global);
}

fn funDeclaration(parser: *Parser) !void {
    const global = try parseVariable(parser, "Expect function name");
    markInitialized(parser);
    try function(parser, .FUNCTION, try ObjString.init(parser.prev.lexeme, parser.object_allocator));
    try defineVariable(parser, global);
}

fn function(parser: *Parser, function_type: FunctionType, function_name: *ObjString) !void {
    _ = try Compiler.init(parser.object_allocator, function_type, function_name);
    parser.compiler.beginScope();
    parser.consume(.LEFT_PAREN, "Expect '(' after function name");
    if (!parser.check(.RIGHT_PAREN)) {
        current_compiler.?.function.arity += 1;
        const param_id = try parseVariable(parser, "Expect parameter name");
        try defineVariable(parser, param_id);
        while (parser.match(.COMMA)) {
            current_compiler.?.function.arity += 1;
            if (current_compiler.?.function.arity > 255) {
                errorAt(parser.current, "Can't have more than 255 parameters");
            }
            const param_id_loop = try parseVariable(parser, "Expect parameter name");
            try defineVariable(parser, param_id_loop);
        }
    }
    parser.consume(.RIGHT_PAREN, "Expect ')' after function parameters");
    parser.consume(.LEFT_BRACE, "Expect '{' before function body");
    try blockStatement(parser);

    const func = try endCompiler(parser);
    try emitOpcodes(@intFromEnum(OpCode.OP_CONSTANT), try makeConstant(Value.initObject(Object.fromFunction(func))), parser);
}

fn parseVariable(parser: *Parser, msg: []const u8) !u8 {
    parser.consume(.IDENTIFIER, msg);
    try declareVarible(parser);
    if (parser.compiler.scope_depth > 0) {
        return 0;
    }
    return try identifierConstant(parser, parser.prev);
}

fn identifierConstant(parser: *Parser, name: Token) !u8 {
    return try makeConstant(Value.initObject(try Object.initObjString(name.lexeme, parser.object_allocator)));
}

fn declareVarible(parser: *Parser) !void {
    if (parser.compiler.scope_depth == 0) {
        return;
    }
    const name = parser.prev;
    var i: i32 = @intCast(parser.compiler.local_cnt);
    i -= 1;
    while (i >= 0) {
        const local = parser.compiler.locals[@intCast(i)];
        if (local.depth != -1 and local.depth < parser.compiler.scope_depth) {
            break;
        }
        if (std.mem.eql(u8, local.name.lexeme, name.lexeme)) {
            errorAt(parser.prev, "Variable with this name is already exist in this scope");
        }
        i -= 1;
    }
    addLocal(parser, name);
}

fn addLocal(parser: *Parser, name: Token) void {
    if (parser.compiler.local_cnt >= 256) {
        errorAt(parser.current, "Too many local variables in function");
        return;
    }
    var local: *Local = &parser.compiler.locals[parser.compiler.local_cnt];
    local.name = name;
    local.depth = -1;
    parser.compiler.local_cnt += 1;
}

fn defineVariable(parser: *Parser, identifier_constant: u8) !void {
    if (parser.compiler.scope_depth > 0) {
        markInitialized(parser);
        return;
    }
    try emitOpcodes(@intFromEnum(OpCode.OP_DEFINE_GLOBAL), identifier_constant, parser);
}

fn and_(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const and_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP_IF_FALSE));
    try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
    try parsePrecedence(.AND, parser);
    patchJump(parser, and_jump);
}

fn or_(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const else_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP_IF_FALSE));
    const then_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP));
    patchJump(parser, else_jump);
    try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
    try parsePrecedence(.OR, parser);
    patchJump(parser, then_jump);
}

fn markInitialized(parser: *Parser) void {
    if (parser.compiler.scope_depth == 0) {
        return;
    }
    const index = parser.compiler.local_cnt - 1;
    parser.compiler.locals[index].depth = parser.compiler.scope_depth;
}

fn statement(parser: *Parser) CompilerError!void {
    if (parser.match(.PRINT)) {
        printStatement(parser) catch return CompilerError.CompilationError;
    } else if (parser.match(.FOR)) {
        forStatement(parser) catch return CompilerError.CompilationError;
    } else if (parser.match(.IF)) {
        ifStatement(parser) catch return CompilerError.CompilationError;
    } else if (parser.match(.WHILE)) {
        whileStatement(parser) catch return CompilerError.CompilationError;
    } else if (parser.match(.LEFT_BRACE)) {
        parser.compiler.beginScope();
        try blockStatement(parser);
        parser.compiler.endScope(parser) catch return CompilerError.CompilationError;
    } else {
        expressionStatement(parser) catch return CompilerError.CompilationError;
    }
}

fn printStatement(parser: *Parser) !void {
    try expression(parser);
    parser.consume(.SEMICOLON, "Expect ';' after value.");
    try emitOpcode(@intFromEnum(OpCode.OP_PRINT), parser);
}

fn forStatement(parser: *Parser) !void {
    parser.compiler.beginScope();

    parser.consume(.LEFT_PAREN, "Exect '(' after for");
    if (parser.match(.SEMICOLON)) {} else if (parser.match(.VAR)) {
        try varDeclaration(parser);
    } else {
        try expression(parser);
    }
    var loop_start = currentChunk().count();
    var exit_jump: usize = undefined;
    var exit_jump_defined: bool = false;
    if (!parser.match(.SEMICOLON)) {
        try expression(parser);
        parser.consume(.SEMICOLON, "Expect ';' after loop condition");
        exit_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP_IF_FALSE));
        exit_jump_defined = true;
        try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
    }

    if (!parser.match(.RIGHT_PAREN)) {
        const body_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP));
        const increment_start = currentChunk().count();
        try expression(parser);
        try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
        parser.consume(.RIGHT_PAREN, "Expect ')' after for clauses");
        try emitLoop(parser, loop_start);
        loop_start = increment_start;
        patchJump(parser, body_jump);
    }

    try statement(parser);
    try emitLoop(parser, loop_start);
    if (exit_jump_defined) {
        patchJump(parser, exit_jump);
        try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
    }

    try parser.compiler.endScope(parser);
}

fn whileStatement(parser: *Parser) !void {
    const loop_start = currentChunk().count();
    parser.consume(.LEFT_PAREN, "Expect '(' after while");
    try expression(parser);
    parser.consume(.RIGHT_PAREN, "Expect ')' after condition");
    const exit_jump = try emitJump(parser, @intFromEnum(OpCode.OP_JUMP_IF_FALSE));
    try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
    try statement(parser);
    try emitLoop(parser, loop_start);
    patchJump(parser, exit_jump);
    try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
}

fn ifStatement(parser: *Parser) CompilerError!void {
    // type inference died again
    parser.consume(.LEFT_PAREN, "Expect '(' after 'if'");
    expression(parser) catch return CompilerError.CompilationError;
    parser.consume(.RIGHT_PAREN, "Expect ')' after condition");
    const thenJump = emitJump(parser, @intFromEnum(OpCode.OP_JUMP_IF_FALSE)) catch return CompilerError.CompilationError;
    emitOpcode(@intFromEnum(OpCode.OP_POP), parser) catch return CompilerError.CompilationError;
    statement(parser) catch return CompilerError.CompilationError;
    const elseJump = emitJump(parser, @intFromEnum(OpCode.OP_JUMP)) catch return CompilerError.CompilationError;
    patchJump(parser, thenJump);
    emitOpcode(@intFromEnum(OpCode.OP_POP), parser) catch return CompilerError.CompilationError;
    if (parser.match(.ELSE)) {
        statement(parser) catch return CompilerError.CompilationError;
    }
    patchJump(parser, elseJump);
}

fn blockStatement(parser: *Parser) CompilerError!void {
    while (parser.current.type_ != .RIGHT_BRACE and parser.current.type_ != .EOF) {
        // here's type inference died in 0.14, so i must return here my own error (or any concrete error)
        declaration(parser) catch return CompilerError.CompilationError;
    }
    parser.consume(.RIGHT_BRACE, "Expect '}' after block");
}

fn expressionStatement(parser: *Parser) !void {
    try expression(parser);
    parser.consume(.SEMICOLON, "Expect ';' after expression.");
    try emitOpcode(@intFromEnum(OpCode.OP_POP), parser);
}

fn number(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const parsed_value = try std.fmt.parseFloat(f64, parser.prev.lexeme);
    try emitConstant(Value.initNumber(parsed_value), parser);
}

fn unary(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const operator_type = parser.prev.type_;

    try parsePrecedence(.UNARY, parser);

    switch (operator_type) {
        .BANG => try emitOpcode(@intFromEnum(OpCode.OP_NOT), parser),
        .MINUS => try emitOpcode(@intFromEnum(OpCode.OP_NEGATE), parser),
        else => unreachable,
    }
}

fn binary(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const operator_type = parser.prev.type_;
    const rule: *ParseRule = getRule(operator_type);
    try parsePrecedence(rule.precedence, parser);

    switch (operator_type) {
        .BANG_EQUAL => try emitOpcodes(@intFromEnum(OpCode.OP_EQUAL), @intFromEnum(OpCode.OP_NOT), parser),
        .EQUAL_EQUAL => try emitOpcode(@intFromEnum(OpCode.OP_EQUAL), parser),
        .GREATER => try emitOpcode(@intFromEnum(OpCode.OP_GREATER), parser),
        .GREATER_EQUAL => try emitOpcodes(@intFromEnum(OpCode.OP_LESS), @intFromEnum(OpCode.OP_NOT), parser),
        .LESS => try emitOpcode(@intFromEnum(OpCode.OP_LESS), parser),
        .LESS_EQUAL => try emitOpcodes(@intFromEnum(OpCode.OP_GREATER), @intFromEnum(OpCode.OP_NOT), parser),

        .PLUS => try emitOpcode(@intFromEnum(OpCode.OP_ADD), parser),
        .MINUS => try emitOpcode(@intFromEnum(OpCode.OP_SUBSTRUCT), parser),
        .STAR => try emitOpcode(@intFromEnum(OpCode.OP_MULTIPLY), parser),
        .SLASH => try emitOpcode(@intFromEnum(OpCode.OP_DIVIDE), parser),
        else => unreachable,
    }
}

fn call(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const arg_count = try argumentList(parser);
    try emitOpcodes(@intFromEnum(OpCode.OP_CALL), arg_count, parser);
}

fn argumentList(parser: *Parser) !u8 {
    var arg_count: u8 = 0;
    if (!parser.check(.RIGHT_PAREN)) {
        try expression(parser);
        arg_count += 1;
        while (parser.match(.COMMA)) {
            if (arg_count == 255) {
                errorAt(parser.current, "Can't have more than 255 arguments");
            }
            try expression(parser);
            arg_count += 1;
        }
    }
    parser.consume(.RIGHT_PAREN, "Expect '(' after arguments");

    return arg_count;
}

fn literal(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    switch (parser.prev.type_) {
        .FALSE => try emitOpcode(@intFromEnum(OpCode.OP_FALSE), parser),
        .TRUE => try emitOpcode(@intFromEnum(OpCode.OP_TRUE), parser),
        .NIL => try emitOpcode(@intFromEnum(OpCode.OP_NIL), parser),
        else => unreachable,
    }
}

fn string(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    const value = Value.initObject(try object.Object.initObjString(parser.prev.lexeme, parser.object_allocator));
    try emitConstant(value, parser);
}

fn variable(parser: *Parser, can_assign: bool) !void {
    var get_op: u8 = undefined;
    var set_op: u8 = undefined;
    const name = parser.prev;
    var arg: usize = undefined;
    if (parser.compiler.resolve(name)) |index| {
        arg = index;
        get_op = @intFromEnum(OpCode.OP_GET_LOCAL);
        set_op = @intFromEnum(OpCode.OP_SET_LOCAL);
    } else |err| {
        if (err != CompilerError.CantResolve) {
            return err;
        }
        arg = try identifierConstant(parser, name);
        get_op = @intFromEnum(OpCode.OP_GET_GLOBAL);
        set_op = @intFromEnum(OpCode.OP_SET_GLOBAL);
    }
    if (can_assign and parser.match(.EQUAL)) {
        try expression(parser);
        try emitOpcodes(set_op, @intCast(arg), parser);
    } else {
        try emitOpcodes(get_op, @intCast(arg), parser);
    }
}

fn parsePrecedence(precedence: Precedence, parser: *Parser) !void {
    parser.advance();
    const prefixRule = getRule(parser.prev.type_).prefix;
    if (prefixRule == null) {
        errorAt(parser.prev, "Expect expression");
        return;
    }
    const can_assign = @intFromEnum(precedence) <= @intFromEnum(Precedence.ASSIGNMENT);
    try prefixRule.?(parser, can_assign);

    while (@intFromEnum(precedence) <= @intFromEnum(getRule(parser.current.type_).precedence)) {
        parser.advance();
        const infixRule = getRule(parser.prev.type_).infix;
        try infixRule.?(parser, can_assign);
    }
    if (can_assign and parser.match(.EQUAL)) {
        errorAt(parser.current, "Invalid assignment target");
    }
}

fn grouping(parser: *Parser, can_assign: bool) !void {
    _ = can_assign;
    try expression(parser);
    parser.consume(.RIGHT_PAREN, "expect ')' after expression");
}

fn endCompiler(parser: *Parser) !*ObjFunction {
    try emitOpcode(@intFromEnum(OpCode.OP_RETURN), parser);
    const ret = parser.compiler.function;
    if (builtin.mode == .Debug) {
        if (parser.hadError == false) {
            debug.disassembleChunk(
                currentChunk().*,
                if (parser.compiler.function.name != null) parser.compiler.function.name.?.str else "<script>",
            );
        }
    }
    current_compiler = current_compiler.?.enclosing;
    return ret;
}

fn emitOpcode(opcode: u8, parser: *Parser) !void {
    try currentChunk().write(opcode, parser.prev.line);
}

fn emitOpcodes(op1: u8, op2: u8, parser: *Parser) !void {
    try emitOpcode(op1, parser);
    try emitOpcode(op2, parser);
}

fn emitJump(parser: *Parser, jmp: u8) !usize {
    try emitOpcode(jmp, parser);
    try emitOpcode(0xff, parser);
    try emitOpcode(0xff, parser);
    return currentChunk().count() - 2;
}

fn emitConstant(value: Value, parser: *Parser) !void {
    try emitOpcodes(@intFromEnum(OpCode.OP_CONSTANT), try makeConstant(value), parser);
}

fn emitLoop(parser: *Parser, loop_start: usize) !void {
    try emitOpcode(@intFromEnum(OpCode.OP_LOOP), parser);
    const offset = currentChunk().count() - loop_start + 2;
    if (offset > std.math.maxInt(u16)) {
        errorAt(parser.current, "Loop body too large");
    }
    try emitOpcode(@intCast((offset >> 8) & 0xff), parser);
    try emitOpcode(@intCast(offset & 0xff), parser);
}

fn patchJump(parser: *Parser, offset: usize) void {
    const jump = currentChunk().count() - offset - 2;
    if (jump > std.math.maxInt(u16)) {
        errorAt(parser.current, "Too much code to jump over");
    }
    currentChunk().code.items[offset] = @intCast((jump >> 8) & 0xff);
    currentChunk().code.items[offset + 1] = @intCast(jump & 0xff);
}

fn makeConstant(value: Value) !u8 {
    const index: u8 = @intCast(try currentChunk().addConstant(value));
    return index;
}

fn currentChunk() *Chunk {
    // i hope at this moment compiling_chunk is not undefined
    return &current_compiler.?.function.chunk;
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
