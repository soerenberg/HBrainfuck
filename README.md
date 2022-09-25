![ci](https://github.com/soerenberg/HBrainfuck/actions/workflows/ci.yml/badge.svg)

# HBrainfuck

Brainfuck interpreter written in Haskell

App supports streaming Brainfuck code from the command line and entering
commands in a REPL.

## Hello World Example

See the [wikipedia page on Brainfuck](https://en.wikipedia.org/wiki/Brainfuck#Hello_World!)
for an example of "Hello World" in Brainfuck.

Here is how the interpreter parses, evaluates and print the result into the
REPL:

```
brainfuck> ++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.
Hello World!
```
