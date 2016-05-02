===
TML
===

``TML`` is a Unix/Scala program. It has little use for non-Scala users.

``TML`` is a Lightweight Markup Language parser (and Specification). It differs from other lightweight markup languages because it is not concise or invisible. It is based in the parser, and what the methods can do.

 
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
In a REPL, try this (quick test static ``apply'' method), ::

    tml.HTML(<string with TML markup>)

For the input string, markup is needed. The package contains some documentation. Try this, ::

    tml.HTML("=== Some charms\n[a{coffee.fr} Not this] for now. So what?")

Store a reusable parser, ::

    val p = tml.HTML()
    p(<string with TML markup>)
    p.result()
    p.clear()

Use the mini-filereader, built-in to preserve your sanity, ::

    tml.HTML(tml.FileReader("""/home/<some filepath>/TML/text/SPEC"""))

Wait until it goes wrong. Lightweight markup parsers always do.


UML
---
TML includes character-based Unicode markup method(s), called UML, ::

    val res = tml.UML(<string with UML markup>)

Converts simple markup code to curly brackets, trademark symbols, fractions, accented characters etc. See the documentation for details.


Markdown extension
------------------
Convert TML markup to the lightweight markup language `Markdown`_.
 
Don't expect the parser to understand ``Markdown`` tricks such as the break idiom. And don't expect ``Markdown`` to handle ``TML``'s block-level accuracy or systemic mark nesting. But, given the kind of material ``Markdown`` is used for, the parser often returns correct results. 

ok.

.. _Markdown: https://daringfireball.net/projects/markdown/
