polysoup
========

The library provides combinators for lazy, incremental XML parsing.
Parsing results are generated lazily and the input is read on demand.

It is built on top of the tagsoup library, which is responsible for
preliminary tokenization of an XML input, and the polyparse library,
which provides primitives for lazy and incremental parsing.

See the [Text.XML.PolySoup][polysoup-module] module for a detailed
API description.


[polysoup-module]: http://hackage.haskell.org/package/polysoup/docs/Text-XML-PolySoup.html "Text.XML.PolySoup"
