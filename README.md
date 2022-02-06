# Latte - Compiler

Compiler of Java-like language written in Haskell. It generates `x86` assembly code and assembles it to binary with `gcc`.

## Latte language

Latte programs can be trivially translated to Java and everyone with basic knowledge of Java can read and understand Latte programs. Among feateures implemented in my compiler are:
- classes with inheritance
- objects
- virtual method dispatch
- arrays

Compiler detects all syntax and logical errors (such as usage of undefined identifier) with provided programs and reports them to the user.

## Project structure
- `src/main` - main file
- `src/latte/Frontend` - frontend implementation - detects all syntax and logical errors (such as usage of undefined identifier) with provided programs and reports them to the user.
- `src/latte/Backend` - backend implementation - generates x86 assembly code
- `src/parser` - BNFC parser files

## Building 
Latte compiler can be built with `make`. It generates `latc_x86` binary in main catalogue.


