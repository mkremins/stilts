# Stilts

Stilts is a tiny Clojure interpreter written in ClojureScript.

## Why?

Stilts was originally born out of my continued frustration with the absence of `eval` in ClojureScript. In this sense it's a sort of twin project to [Ceci](https://github.com/mkremins/ceci), my more-straightforward attempt to create a ClojureScript compiler that can function independently of the JVM and hopefully one day become capable of compiling itself.

I'm also interested in exploring some of the ideas put forth by projects like [EClj](https://github.com/brandonbloom/eclj) and [Kiss](https://github.com/mikera/kiss), in particular that of the functionally pure embedded interpreter. Does it become safer or more practical to embed scripting engines in larger apps when evaluation is necessarily free from side-effects and the evaluation environment can be precisely controlled? I'm not sure yet, but I'm betting the answer will be a resounding "yes".

## Features

### Stuff I have
* Support for `def`, `do`, `if`, `fn*`, `let*`, `loop*`, `quote`, `recur`, `throw`, `try` special forms
* Macroexpansion and runtime `defmacro`
* Precise control over the evaluation environment
* A [command-line REPL](https://github.com/mkremins/stilts-cli) via node.js

### Stuff I want
* See the [issue tracker](https://github.com/mkremins/stilts/issues?state=open)

## Usage

Don't.

## License

[MIT License](http://opensource.org/licenses/MIT). Hack away.
