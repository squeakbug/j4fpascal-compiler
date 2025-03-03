# Parsing

## Single pass compilers, old machines and Vaughan Pratt’s "top-down operator precedence parsing" (Анализатор Уорли)

This method of parsing doesn't require building AST for code emition

Pascal and C compilers were initially single-parse compilers, so parsing, analysis, and code generation produce output code directly in the parser, without ever allocating any
syntax trees or other IRs. But i won't do it.

## Overengineering

Why do people use tools like Yacc, Flex, ANTLR, etc.? Recursive descent parsing is often more convenient. For implementing a toy language, you would likely use a parser combinator library or handwritten tools. In practice, this is often the case. Unfortunately, even in universities' compiler introduction courses, there are people who think differently.

> All of us suffer from the vice of “when all you have is a hammer, everything looks like a nail”, but perhaps none so visibly as compiler people. You wouldn’t believe the breadth of software problems that miraculously seem to require a new little language in their solution as soon as you ask a compiler hacker for help.
> 
> Yacc and other compiler-compilers are the most delightfully recursive example. “Wow, writing compilers is a chore. I know, let’s write a compiler to write our compiler for us.” 
> 
> For the record, I don’t claim immunity to this affliction.
> 
> *Crafting interpreters*

-- --

* [Example for C compiler](https://github.com/ssloy/tinycompiler) и [статья на Хабре](https://habr.com/ru/articles/786158/) с описанием проекта. Здесь называется анализатором Уорли
* [Crafting interpreters](https://craftinginterpreters.com/). ЗЗдесь называется парсером Пратта
