# j4fpascal-compiler

*Are you ready for pain? Are you ready for suffering? If the answer is yes, then you're ready for writing compiler on Rust™ from scratch*

## 

Pascal and C compilers were initially single-parse compilers, so parsing, analysis, and code generation produce output code directly in the parser, without ever allocating any
syntax trees or other IRs. But i won't do it.

Why do people use tools like Yacc, Flex, ANTLR, etc.? Recursive descent parsing is often more convenient. For implementing a toy language, you would likely use a parser combinator library or handwritten tools. In practice, this is often the case. Unfortunately, in universities, there are people who think differently.

## Links

* [Rust compiler in Rust](https://github.com/rust-lang/rust)
* [gleam compiler in Rust](https://github.com/gleam-lang/gleam)

* [Poline](https://github.com/cronokirby/poline)
* [C compiler](https://github.com/ClementTsang/rustcc)

## Literature

* [Red dragon]()
* [Crafting interpreters]()
* [Мучник]()
* []()
