language-java 
=============

[![Build Status](https://travis-ci.org/vincenthz/language-java.png?branch=master)](https://travis-ci.org/vincenthz/language-java)
[![BSD](http://b.repl.ca/v1/license-BSD-blue.png)](http://en.wikipedia.org/wiki/BSD_licenses)
[![Haskell](http://b.repl.ca/v1/language-haskell-lightgrey.png)](http://haskell.org)

Haskell parser and pretty printer for the java language.


How to use
----------

Simple compilation unit parser:

    parser compilationUnit "import java.util.*; public class MyClass {}"

or from a file:

    ast <- parser compilationUnit `fmap` readFile "myClass.java"
