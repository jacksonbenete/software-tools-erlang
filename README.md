# Software Tools in Erlang
This is an implementation of various Unix common tools, as taught on `Software Tools in Pascal` by Kernighan and Plauger. This is my interpretation of a Erlang implementation.

## Important
You need to compile `ststd.erl` first before trying any tools, as almost or all of them will make heavy use of functions like `getc` and `putc`.

Indentation is made by a code formatter, if any file is lacking indentation I'm sorry, please notify me or PR.

## File Structure
There are redundant files.
All the tools can be found on `stcN.erl` files, where `N` is the chapter number. So you can compile and call the stc1 module to have all chapter one tools in one place.
That's easier than having to compile each single tool separately.
In those files there is a `-compile(export_all)` flag that helps in testing functions of different arities.

Using a function from the `stcN` packages inside repl means entering the Input yourself through an arity 0 function, or passing an Input through an arity 1 function.

All the tools will be provided as escript as well, when possible, to accept command line arguments such as a filename, or to receive input from terminal.

## Todo
- translit still needs to implement `^` (all but), and `@` (escape characters). Shouldn't be difficult but might need another approach than maps.
- print (3.6 Multi-stage Processing: Pipelines)
- makecopy (3.7 Creating Files Dinamically)
- archive (3.8 Putting it All Together: archive)
- sorting methods (the entire chapter 4)
