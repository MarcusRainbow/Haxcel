# Haxcel

An addin for running Haskell within Excel.

## How it works

GHCI is the command-line interpreter for Glasgow Haskell. From the command line you can execute Haskell code, control the Haskell environment and load/reload/unload modules. It is possible to assign values, functions and other data structures to labels, which can be used in future command line actions.

This addin launches GHCI in a separate process, but controlling its stdin and stdout. This means that from within Excel, we can pass commands to Haskell, assigning and storing values, functions etc, which can then be used from other cells.

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

You can use hxAssign to define functions, but the definition must be of the form label = expression. This means functions must be defined using lambda expressions. (We may add syntactic sugar methods to haxcel to support function definition in prettier ways.)

hxAssign returns the name (the first arg), in the case of success. This makes it easy to chain dependencies in a natural way. Note that Excel does not support cyclic dependency structures, as you could get with corecursion in Haskell. In the case of error, it returns whatever error GHCI wrote, as a string.

The expression in hxAssign (the second arg) can contain embedded references to other variables, specified as "{}" as in Rust or Python. Eventually, we shall support format specifiers inside the braces, but not yet. These refer to the third and subsequent args, which means you can easily maintain the dependency structure of your Haskell definitions.

### hxShow

```Excel
=hxShow("add_three_cells + {} + {} + {}", B1, B2, B3)
```

hxShow allows you to show inside Excel the result of a Haskell calculation. If the calculation simply returns a scalar or a string, this is the return value of hxShow and appears in the cell. If the calculation errored out, the error message appears in the cell.

As with hxAssign, you can embed "{}" arguments in the expression.

Where things get clever is if the expression returns a list or a list of lists. Haxcel finds out the number of cells you are trying to fill with the formula and executes "take" on the expression, so that only the minimum calculation is executed.

For example, you could write:

```Excel
=hxShow("ints_from 0 where ints_from n = [n..] : (ints_from (n+1))")
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
