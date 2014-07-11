# Terp

Terp is a tiny ClojureScript interpreter written in ClojureScript.

Yes, I know the name is terrible. It will be changed.

## Why?

I still want `eval` for ClojureScript. I still don't have `eval` for ClojureScript. Hopefully this will get me there so I can avoid writing a compiler and move on to more interesting stuff.

## Features

### Stuff I have
* Support for `def`, `do`, `if`, `fn*`, `let*`, `loop*`, `quote`, `recur` special forms
* Macroexpansion
* Runtime `defmacro` (in theory)
* Precise control over the evaluation environment
* A command-line REPL via node.js

### Stuff I want
* Support for `throw`, `try` special forms
* Support for more `clojure.core` macros
* JS interop of some sort
* Multiple-arity functions
* `deftype`, `defprotocol`, `defrecord`

### Stuff I don't want to think about right now 
* Namespaces
* Parsing (just use `cljs.reader`)

## Usage

Don't.
