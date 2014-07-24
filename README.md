# Stilts

Stilts is a tiny ClojureScript interpreter written in ClojureScript.

## Why?

I still want `eval` for ClojureScript. I still don't have `eval` for ClojureScript. Hopefully this will get me there so I can avoid writing a compiler and move on to more interesting stuff.

## Features

### Stuff I have
* Support for `def`, `do`, `if`, `fn*`, `let*`, `loop*`, `quote`, `recur`, `throw`, `try` special forms
* Macroexpansion and runtime `defmacro`
* Precise control over the evaluation environment
* A command-line REPL via node.js
* JS interop via `cljs.core/aget`

### Stuff I want
* See the [issue tracker](https://github.com/mkremins/stilts/issues?state=open)

## Usage

Don't.

## License

[MIT License](http://opensource.org/licenses/MIT). Hack away.
