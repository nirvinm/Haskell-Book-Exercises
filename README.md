# My solutions to exercises from the book "Haskell Programming: From First Principles"

http://haskellbook.com/

Book authors
------------
Christopher Allen (https://twitter.com/bitemyapp)

Julie Moronuki (https://twitter.com/argumatronic)

***Note: This is an unoffical repository. This repo is in no way connected to the original book or authors. Code may have bugs or may not solve the problems completely. Learn at your own risk.***

Compiling this repository
-------------------------

Each chapter is divided into seperate `Stack` project. All the projects are compiled with Stackage snapshot version `LTS 13.18`. Initial few chapters don't have Stack project setup for brevity. Those chapters contain solutions in individual Haskell files. They can be simply loaded with `GHCI`.


**To load individual Haskell file**

> $ stack ghci <filename.hs>

**To compile Stack project**

> $ stack build

**To load Stack project in `GHCI`**

> $ stack ghci

**To run `QuickCheck` tests**

> $ stack test


Other Haskell Book solutions
----------------------------

https://github.com/larrybotha/haskell-book

https://github.com/dmvianna/haskellbook

