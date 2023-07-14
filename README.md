# Brainfuck

A toy [Brainfuck](https://en.wikipedia.org/wiki/Brainfuck) interpreter written
in Haskell

## Why?

Why not.

## Installation

Install Brainfuck somewhere in your $PATH for convenience:

```shell
cabal install --install-dir ~/.local/bin
```

## Usage

Save a Brainfuck program in a file and run `Brainfuck <path/to/file>`.
Or put `#!/usr/bin/env Brainfuck` at the top of that file and run it as a script.

Some example programs are available in the [examples](./examples) directory.

```shell
$ ./examples/hello_world.bf
Hello World!

$ echo 'Hello there, General Kenobi' | ./examples/rot13.bf
Uryyb gurer, Trareny Xrabov

$ echo 'Hello there, General Kenobi' | ./examples/rot13.bf | ./examples/rot13.bf
Hello there, General Kenobi

$ ./examples/cat.bf < Setup.hs
import Distribution.Simple
main = defaultMain
```
