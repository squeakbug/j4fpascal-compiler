# j4fpascal-compiler

*Are you ready for pain? Are you ready for suffering? If the answer is yes, then you're ready for writing compiler on Rust™ from scratch*

## 

Pascal and C compilers were initially single-parse compilers, so parsing, analysis, and code generation produce output code directly in the parser, without ever allocating any
syntax trees or other IRs. But i won't do it.

Why do people use tools like Yacc, Flex, ANTLR, etc.? Recursive descent parsing is often more convenient. For implementing a toy language, you would likely use a parser combinator library or handwritten tools. In practice, this is often the case. Unfortunately, in universities, there are people who think differently.

## Links

* [Rust compiler in Rust](https://github.com/rust-lang/rust)
* [gleam compiler in Rust](https://github.com/gleam-lang/gleam)
* [Free Pascal Compiler](https://gitlab.com/freepascal.org)

* [Poline](https://github.com/cronokirby/poline)
* [C compiler](https://github.com/ClementTsang/rustcc)

## Literature

* [Crafting interpreters](https://craftinginterpreters.com/)
* [Engineering: A Compiler](https://www.amazon.com/Engineering-Compiler-Keith-Cooper/dp/012088478X) - there is 3rd edition
* [Мучник](https://www.amazon.com/Advanced-Compiler-Design-Implementation-Muchnick/dp/1558603204)
* [Red dragon](https://www.amazon.com/Compilers-Principles-Techniques-Tools-2nd/dp/0321486811)
* [Оптимизирующие компиляторы. Структура и алгоритмы](https://www.chitai-gorod.ru/product/optimiziruyushchie-kompilyatory-struktura-i-algoritmy-3059667)
