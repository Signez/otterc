otterc: a simple compiler for a fictional language
==================================================

Do you know the _Otter Programming Language_? No, you didn't miss a thing on Hacker News; it's simply
a simple fictional language, forked from TOOL, another fictional language created at EPFL for these
[Compiler Construction course](http://lara.epfl.ch/w/cc12:top).

`otterc` is a Scala compiler that takes an `.otr` (Otter) file and generates corresponding `.class`
(JVM bytecode). It is heavily based on EPFL original material that you can see [here](http://lara.epfl.ch/w/cc12:top).

Otter Grammar
-------------

```
    Goal              ::=  MainObject ( ClassDeclaration )* <EOF>
    MainObject        ::=  'object' Identifier '{' 'def' 'main' '(' ')' ':' 'Unit' '=' '{' Statement '}' '}'
    ClassDeclaration  ::=  'class' Identifier '(' extends Identifier )? '{' ( VarDeclaration )* ( MethodDeclaration )* '}'
    VarDeclaration    ::=  'var' Identifier ':' Type ';'
    MethodDeclaration ::=  'def' Identifier '(' ( Identifier ':' Type ( ',' Identifier ':' Type )* )? ) ':' Type '=' '{' ( VarDeclaration )* ( Statement )* 'return' Expression ';' '}'
    Type              ::=  'Int' '[' ']'
                           'Bool'
                           'Int'
                           'String'
                           Identifier
    Statement         ::=  '{' ( Statement )* '}'
                           'if' '(' Expression ')' Statement ( 'else' Statement )?
                           'while' '(' Expression ')' Statement
                           'println' '(' Expression ) ';' ?
                           Expression ';' ?
    Expression        ::=  Expression ( '&&' | '||' | '==' | '<' | '+' | '-' | '*' | '/' ) Expression
                           Expression '[' Expression ']'
                           Expression '.' 'length'
                           Expression '.' Identifier '(' ( Expression ( ',' Expression )* )? ')'
                           <INTEGER_LITERAL>
                           '"' <STRING_LITERAL> '"'
                           'true'
                           'false'
                           Identifier
                           'this'
                           'new' 'Int' '[' Expression ']'
                           'new' Identifier '(' ')'
                           '!' Expression
                           Identifier '[' Expression ']' '=' Expression
                           Identifier '=' Expression
                           '(' Expression ')'
    Identifier        ::=  <IDENTIFIER>
```

* `<IDENTIFIER>` represents a sequence of letters, digits and underscores, starting with a letter and which is not a keyword. Identifiers are case-sensitive.
* `<INTEGER_LITERAL>` represents a sequence of digits, with no leading zeros
* `<STRING_LITERAL>` represents a sequence of arbitrary characters, except new lines and **%%"%%**. There is no support for escape characters such as **\n**.
* `<EOF>` represents the special end-of-file character.

Building otterc
---------------

Install [Gradle](http://www.gradle.org/) and run `gradle distZip` on project root. An archive will be generated
into `build/distributions` ; unzip it anywhere and run `bin/otterc`. That's it.

More information
----------------

Automatically generated documentation can be found in `doc` after running `gradle scaladoc`.

You can see related slides on my [Speaker Deck](https://speakerdeck.com/signez/otter-project-french) if
you understand french language.

This was created during a specially crafted project at [INSA de Lyon](http://if.insa-lyon.fr/).

License
-------

- [Attribution-ShareAlike 2.5 Generic (CC BY-SA 2.5) Creative Commons Licence](http://creativecommons.org/licenses/by-sa/2.5/)
- Otter Compiler (`otterc`): Stanislas Signoud (@Signez).
- Original Authors (`toolc`): Viktor Kuncak, Eva Darulova, Etienne Kneuss, Ivan Kuraj. _Thank you again for this awesome course!_