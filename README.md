# ECE-466: Compilers - Spring 2021

This repository contains the work done throughout the semester which is a compiler which translates a subset of C to X86-32 assembly.

To build the entire compiler run 'make backend'. Then run build/backend. The resulting assembly file will be in out.S and can be run via 'cc -m32 out.S && ./a.out'.

To build just the lexer run 'make lexer'. To build just the parser run 'make parser'. To build everything including quad generation run 'make quads'.

To run the lexer run `build/lexer`.

To run the parser up to quad generation run `build/parser`.

To run everything up to quad generation run 'build/quads'.

Each component has an optional argument which represents the filename of a file in which the input will be read from. Otherwise input is read from stdin.
