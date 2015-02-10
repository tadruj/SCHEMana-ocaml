SCHEMana
========

[![Made at Hacker School](http://img.shields.io/badge/Made_At-Hacker_School-brightgreen.svg)](https://www.hackerschool.com)

This a SCHEME implementation 

in OCaml
--------

written entirely for my educational purposes. This means that it's deliberately incomplete. My goal was to learn about interpreting lexical scopes, functions and anonymous functions, parsers, monads and give a spin to OCaml which is a great possible candidate for super-fast and air-tight Bitcoin trading workers.

I had quite a few challenges and I had wished for more references on GitHUB and across the web. This is one of the reasons I'm putting this here. Some of the challenges:

#### eager evaluation model

because of eager evaluation nature of OCaml I had quite some fun tying the recursion knot thus helping myself with `lazy()` and `force()` a lot. I was pair programming with [davidk01] on this.

[davidk01]: https://github.com/davidk01

#### point free parsers

I really like the readability of parsers written in point free notation, so I strived to implement all the parsers in this form. I'm thankful to [sean-san] for the wise nods.

[sean-san]: https://github.com/sgrove

#### monadic parsing

My first touch with monads was proof-checked by an eagle eye of [yamada-san]. I later melted the monad stigma with Haskell and went on implementing some in JavaScript.

[yamada-san]: https://github.com/yamadapc

# Install

Install OCaml via [OPAM]. Read more about OCaml and its possible stack in [Real World OCaml][rwo] book.

    opam install core core_extended

install [mParser] with: 

	opam install mparser

[mParser] depends on PCRE-OCaml library which is FFI bound to a C library so `js_of_ocaml` will not be able to produce anything immediately useful. 

[OPAM]: http://opam.ocaml.org/
[mParser]: https://bitbucket.org/cakeplus/mparser/
[rwo]: https://github.com/realworldocaml/book/wiki/Installation-Instructions

# Build

`corebuild -pkg mparser src/schemana.native`

# Run

`schemana.native` for REPL

or

`schemana.native <scheme-command>` to run a single SCHEME command

or

`schemana.native << scheme-file.scm` to interpret a SCHEME script

# What's missing
- exception handling
- dotted-list implementation
- standard-library
