
## A bytecode virtual machine for lox programming language written in zig

[crafting interpreters book](https://www.craftinginterpreters.com/a-bytecode-virtual-machine.html)

## build&run

supports latest zig stable release

```bash
zig build && zig-out/bin/loxvm filename.lox # to run debug version
```
```bash
zig build -Doptimize=ReleaseFast && zig-out/bin/loxvm filename.lox # to run optimized version
```

## supports
- [x] expressions
- [x] statements
- [x] control flow (branches, loops)
- [x] functions
- [x] closures
- [x] garbage collection (works only in Debug and ReleaseFast modes)
- [x] classes, objects and methods
- [x] inheritance, 'super' calls


## the book is finished





