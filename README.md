# Haxcel

An addin for running Haskell within Excel.

## How it works

GHCI is the command-line interpreter for Glasgow Haskell. From the command line you can execute Haskell code, control the Haskell environment and load/reload/unload modules. It is possible to assign values, functions and other data structures to labels, which can be used in future command line actions.

This addin launches GHCI in a separate process, but controlling its stdin and stdout. This means that from within Excel, we can pass commands to Haskell, assigning and storing values, functions etc, which can then be used from other cells.
