# Haxcel

An addin for running Haskell within Excel.

## Motivation

In some senses, Excel is a functional language. Functions in cells generally do not have side-effects. Flow of control expressions such as IF are functions, as is everything else. However, some things are missing, and for these, an embedded genuinely functional language such as Haskell is helpful. Moreover, because Excel is itself partly functional, Haskell embeds in a really natural way, so you can easily use both Excel and Haskell functions together in the same sheet.

* In Haxcel, functions are first-class objects, so you can create a function at runtime, store it in an Excel cell and pass it to other funtions.
* In Haxcel, function calls are non-strict, which makes it possible to handle infinitely recursive functions and infinite lists. The functions are only evaluated to the extent that they fill the cells the user wants them to fill, and they are only evaluated when they are needed.
* Haxcel allows you to load and compile Haskell modules, which opens the possibility of really high-performance computing in a spreadsheet. It also allows you to use Haskell libraries, for specialised tasks such as financial maths.
* Haxcel optionally works with floating point numbers, which interface well with native Excel, or strings, which support Haskell types such as Integer (integers of unlimited size), Rational (exact representation of rational numbers), Complex etc.

## How it works

GHCI is the command-line interpreter for Glasgow Haskell. From the command line you can execute Haskell code, control the Haskell environment and load/reload/unload modules. It is possible to assign values, functions and other data structures to labels, which can be used in future command line actions.

This addin launches GHCI in a separate process, but controlling its stdin and stdout. This means that from within Excel, we can pass commands to Haskell, assigning and storing values, functions etc, which can then be used from other cells. In the unlikely event of Haskell crashing, it will not bring down Excel. Something I am working on is the ability to interrupt a slow-running or infinite Haskell function, so that Excel can continue and Haskell can be restarted.

## How to use it

The key functions are hxAssign and hxShow. These are the equivalent in Haskell of expressions like "a = foo x" and "foo x".

You need to be aware that Excel is itself a kind of functional language. Formulae in cells are first inspected to find any inter-cell dependencies, so that Excel can produce an acyclic graph, much like the AST in a functional language such as Haskell.

Much like Haskell, expressions are not evaluated in left-right top-bottom order, but are evaluated non-strictly in the order specified by the dependency graph.

What this means is that you have to be careful to get the inter-cell dependencies right. For example, if you want to use a variable you have assigned using hxAssign, you need to make sure that hxAssign cell is evaluated before the place you use the variable. To make this easier, methods like hxAssign take multiple parameters, so you can point at the cells that define the variables you use. If hxAssign succeeds, it writes the name of its assigned variable into the cell.

### hxAssign

```Excel
=hxAssign("add_three_cells", "{} + {} + {}", A1, A2, A3)
```

hxAssign allows you to assign a name to a Haskell expression. The expression can be a number, string, list or a function -- anything that Haskell can assign a name to. The function is typically not evaluated unless it is marked as strict. Thus it is perfectly legal to write something like hxAssign("all_positive_integers", "[0..]"). This would take forever to execute as it stands, but non-strict evaluation means only as much of it is evaluated as is needed, when it is needed.

You can use hxAssign to define functions, but the definition must be of the form label = expression. This means functions must be defined using lambda expressions. (We may add syntactic sugar methods to Haxcel to support function definition in prettier ways.)

hxAssign returns the name (the first arg), in the case of success. This makes it easy to chain dependencies in a natural way. Note that Excel does not support cyclic dependency structures, as you could get with mutual recursion in Haskell. Mutual recursion can be set up in modules loaded into Excel, or by using string labels to break the dependecy cycles. In the case of error, hxAssign returns whatever error GHCI returned, as a string.

The expression in hxAssign (the second arg) can contain embedded references to other variables, specified as "{}" as in Rust or Python. Eventually, we shall support format specifiers inside the braces, but not yet. These refer to the third and subsequent args, which means you can easily maintain the dependency structure of your Haskell definitions.

### hxExec

```Excel
=hxExec("add_three_cells + {} + {} + {}", B1, B2, B3)
```

hxExec allows you to show inside Excel the result of a Haskell calculation. If the calculation simply returns a scalar or a string, this is the return value of hxExec and appears in the cell. If the calculation errored out, the error message appears in the cell.

As with hxAssign, you can embed "{}" arguments in the expression.

Where things get clever is if the expression returns a list or a list of lists. Haxcel finds out the number of cells you are trying to fill with the formula and executes "take" on the expression, so that only the minimum calculation is executed.

For example, you could write:

```Excel
=hxExec("ints_from 0 where ints_from n = [n..] : (ints_from (n+1))")
```

This expression returns an infinite list of infinite lists. If you invoke this expression as an array formula (Ctrl+Shift+Enter) in a 3x4 range of cells, it actually invokes:

```haskell
hx_temp = ints_from 0 where ints_from n = [n..] : (ints_from (n+1))
:t hx_temp -- this finds that the expression is a list of lists
take 4 (map (take 3) hx_temp)
```

This makes it harder to call something in Haskell that will never return. Though you still need to be careful -- some functions are very slow, and lists of lists of infinite lists need to be handled with care.

### Loading modules

The functions for this are hxLoad and hxReload, which map to ":l" and ":r" in GHCI. The current directory is defined in your Excel settings, typically the "Documents" directory, so you probably want to supply a full path to the Haskell file to be loaded.

Be careful with dependencies. The sample Excel file, test.xlsx, shows one way to ensure a module is loaded before the functions it defines are invoked.

### Some other functions

| Function      | Args           | Behaviour  |
| ------------- |--------------- | ---------- |
| hxShow        | value, args... | Same as hxExec except it always writes strings |
| hxRaw         | command        | Submits a command direct to GHCI and returns the response |
| hxVersion     |                | Returns version information about Haxcel |
| hxGHCIVersion |                | Returns version information about GHCI |
| hxLoggingOn   |                | Turns on logging to OutputDebugString (use DebugView to view it) |
| hxLoggingOff  |                | Turns off logging |

There are also functions for just writing to GHCI, or reading its stdout or stderr streams, but these should be treated with extreme care, as they can leave GHCI in an unstable state, or hang waiting for ever.

## Installing Haxcel

Haxcel is written in Rust, and runs in Excel in Win64. (It may also run in 32bit Windows, but I have not tested that combination.) You therefore need a 64bit Windows machine with Microsoft Office. You also need a Win64 version of the Glasgow Haskell Compiler (GHC). Specifically, Haxcel uses GHCi in an interactive session, connecting with Windows Pipes.

Install Rust for a Win64 environment (i.e. not Windows Subsystem Linux), including cargo.

Haxcel depends on a number of other Rust projects (see the Cargo.toml file for details), but these should automatically install. You may need to install xladd manually, from this Git account. Again, this is a Rust project, with its own cargo file.

Build Haxcel (and if necessary xladd) by running "cargo build install" from the directory containing Cargo.toml. The result of the build is a file Haxcel.dll. Where this ends up will depend on whether you build in debug or release, and what you call the directory containing Haxcel, but for example it may end up in "Haxcel\target\debug".

One way to load it is to first load the sample spreadsheet, tests.xlsx. If you hit f9 to recalculate, you will see lots of error messages, as Haxcel is not loaded. Now load the dll by typing its full path into the File Open dialog. Excel will recognise that you are loading an unsigned Excel addin, and will warn you about the security risks. Override these and Excel will load Haxcel, and also start GHCi and connect Excel to GHCi using pipes.

Now you can recalculate the spreadsheet, by selecting each cell and hitting f2 Enter, or by hitting f9 to recalculate the whole spreadsheet. You should see the correct calculation. 
