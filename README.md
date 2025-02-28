# j4fpascal-compiler

*Are you ready for pain? Are you ready for suffering? If the answer is yes, then you're ready for writing compiler on Rust™ from scratch*

## Single pass compilers, old machines and Vaughan Pratt’s "top-down operator precedence parsing"

Pascal and C compilers were initially single-parse compilers, so parsing, analysis, and code generation produce output code directly in the parser, without ever allocating any
syntax trees or other IRs. But i won't do it.

## Overengineering

Why do people use tools like Yacc, Flex, ANTLR, etc.? Recursive descent parsing is often more convenient. For implementing a toy language, you would likely use a parser combinator library or handwritten tools. In practice, this is often the case. Unfortunately, in universities, there are people who think differently.

```
All of us suffer from the vice of “when all you have is a hammer, everything looks like a nail”, but perhaps none so visibly as compiler people. You wouldn’t believe the breadth of software problems that miraculously seem to require a new little language in their solution as soon as you ask a compiler hacker for help.

Yacc and other compiler-compilers are the most delightfully recursive example. “Wow, writing compilers is a chore. I know, let’s write a compiler to write our compiler for us.” 

For the record, I don’t claim immunity to this
affliction.
```

## Links

* [Rust compiler in Rust](https://github.com/rust-lang/rust)
* [gleam compiler in Rust](https://github.com/gleam-lang/gleam)
* [Free Pascal Compiler](https://gitlab.com/freepascal.org)
* [QEMU TCG compiler](https://github.com/qemu/qemu/blob/master/tcg/tcg.c)

* [Poline](https://github.com/cronokirby/poline)
* [C compiler](https://github.com/ClementTsang/rustcc)

## Literature

* [Crafting interpreters](https://craftinginterpreters.com/)
* [Engineering: A Compiler](https://www.amazon.com/Engineering-Compiler-Keith-Cooper/dp/012088478X) - there is 3rd edition
* [Мучник](https://www.amazon.com/Advanced-Compiler-Design-Implementation-Muchnick/dp/1558603204)
* [Red dragon](https://www.amazon.com/Compilers-Principles-Techniques-Tools-2nd/dp/0321486811)
* [Compiler Construction. Niklaus Wirth](https://www.amazon.com/Compiler-Construction-International-Computer-Science/dp/0201403536)
* [Оптимизирующие компиляторы. Структура и алгоритмы](https://www.chitai-gorod.ru/product/optimiziruyushchie-kompilyatory-struktura-i-algoritmy-3059667)

## Language virtual machines

https://en.wikipedia.org/wiki/Pascal_MicroEngine - Pascal MicroEngine is a series of microcomputer products manufactured by Western Digital from 1979 through the mid-1980s, designed specifically to run the UCSD p-System efficiently.
https://en.wikipedia.org/wiki/UCSD_Pascal 
https://en.wikipedia.org/wiki/ZPU_(processor) - The ZPU is a microprocessor stack machine designed by Norwegian company Zylin AS to run supervisory code in electronic systems that include a field-programmable gate array (FPGA). Don't be confused with https://zipcpu.com/
https://github.com/OpenSmalltalk/opensmalltalk-vm
https://docs.oracle.com/javase/specs/jvms/se8/html/
https://github.com/happi/theBeamBook
https://dl.acm.org/doi/pdf/10.1145/99370.99385
https://web.archive.org/web/20090131093645/http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-335.pdf

