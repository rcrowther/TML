===
TML
===

``TML`` is a Unix/Scala program. It has little use for non-Scala users.

``TML`` is a Lightweight Markup Language and parser. It differs from other lightweight markup languages because it is not concise or invisible. It is based round the parser, and what a few methods can do.

 
Usage
=====
Needs
-----
Java and Scala. Both standard, OpenJDK is fine.

Building
--------
I don't like build tools, and have no base to distribute Java .jar files. So building is by ``ssc``.

It may be possible to coax ``SBT`` into compiling ``TML``.

Running
-------
In a REPL, try this, ::

    val p = new tml.HTML()
    p.parse(<string with TML markup>)
    p.result()
    p.clear()

For the input string, markup is needed. The package contains some documentation. Try this (on the quick test ``apply'' method), ::

    p("=== Some charms\n[a{coffee.fr} Not this] for now. So what?")

UML
---
TML includes Unicode character markup method(s), called UML, ::

    val res = tml.UML(<string with UML markup>)

Converts simple markup code to curly brackets, trademark symbols, fractions, accented characters etc. See the documentation for details.

ok.
