"YACC is dead" is the name of the paper that originally presented this approach to parsing. This is a derivitive parser, which (while having exponential time complexity in the worst case) generally performs well, can handle any context-free grammar, returns parse trees (so that abiguity can be handled after the fact), and is easy to implement and understand.

At the highest level, there is DEFSYNTAX and PARSE-RULE, which make writing a parser for your object model very simple.

To specify a parse rule, you use characters, CHOICE, ~ (concatenation), and *+ (repetition).
