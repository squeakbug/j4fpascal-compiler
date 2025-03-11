# References

## Links

### Rust-specific 

* [Rust compiler in Rust](https://github.com/rust-lang/rust) - check many tools and utils
* [gleam compiler in Rust](https://github.com/gleam-lang/gleam) - check good parser and diagnostic
* [ante compiler in Rust](https://github.com/jfecher/ante) - simple for reading code of compiler for rust-like language. Language provides ownership concept
* [Python interpreter in Rust](https://github.com/RustPython/RustPython)
    * Parser lives in separate [crate](https://github.com/RustPython/Parser) cause it's used by [ruff](https://github.com/astral-sh/ruff) project too
    * [Bytecode definition](https://github.com/RustPython/RustPython/blob/main/compiler/core/src/bytecode.rs)
    * [PyObject definition](https://github.com/RustPython/RustPython/blob/main/vm/src/object/core.rs#L104)
    * [Virtual machine](https://github.com/RustPython/RustPython/tree/main/vm/src/vm)
    * [Some useful utils](https://github.com/RustPython/RustPython/tree/main/common/src)

### Pascal-specific

* [A-Bauman-BTPC-64](https://github.com/bmstu-iu9/A-Bauman-BTPC-64)
* [Free Pascal Compiler](https://gitlab.com/freepascal.org)

### Code analysis and optimizing transformations

* [shecc - A self-hosting and educational C optimizing compiler](https://github.com/sysprog21/shecc)
* [Interpreter for Rust's mid-level intermediate representation](https://github.com/rust-lang/miri)
* [Educational compiler intermediate representation](https://github.com/sampsyo/bril) - splitting phi functions into 'get' and 'set' functions

### Other

* [QEMU TCG compiler](https://github.com/qemu/qemu/blob/master/tcg/tcg.c)

### Literature

* [Crafting interpreters](https://craftinginterpreters.com/)
* [Static Single Assignment Book](https://pfalcon.github.io/ssabook/latest/)
    * [Static Single-Assignment Form Seminar](https://compilers.cs.uni-saarland.de/ssasem/)
* [Engineering: A Compiler](https://www.amazon.com/Engineering-Compiler-Keith-Cooper/dp/012088478X) - there is 3rd edition
* [Мучник](https://www.amazon.com/Advanced-Compiler-Design-Implementation-Muchnick/dp/1558603204)
* [Tiger book (C, Java and ML ed.)](https://www.cs.princeton.edu/~appel/modern/ml)
* [Red dragon](https://www.amazon.com/Compilers-Principles-Techniques-Tools-2nd/dp/0321486811)
* [Compiler Construction. Niklaus Wirth](https://www.amazon.com/Compiler-Construction-International-Computer-Science/dp/0201403536)
* [Оптимизирующие компиляторы. Структура и алгоритмы](https://www.chitai-gorod.ru/product/optimiziruyushchie-kompilyatory-struktura-i-algoritmy-3059667)

### Language virtual machines

https://en.wikipedia.org/wiki/Pascal_MicroEngine - Pascal MicroEngine is a series of microcomputer products manufactured by Western Digital from 1979 through the mid-1980s, designed specifically to run the UCSD p-System efficiently.
https://en.wikipedia.org/wiki/UCSD_Pascal 
https://en.wikipedia.org/wiki/ZPU_(processor) - The ZPU is a microprocessor stack machine designed by Norwegian company Zylin AS to run supervisory code in electronic systems that include a field-programmable gate array (FPGA). Don't be confused with https://zipcpu.com/
https://github.com/OpenSmalltalk/opensmalltalk-vm
https://docs.oracle.com/javase/specs/jvms/se8/html/
https://github.com/happi/theBeamBook
https://dl.acm.org/doi/pdf/10.1145/99370.99385
https://web.archive.org/web/20090131093645/http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-335.pdf