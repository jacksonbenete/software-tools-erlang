# Software Tools in Erlang
This is an implementation of various Unix common tools,
as taught on 'Software Tools in Pascal' by Kernighan 
and Plauger.
This is my interpretation of an Erlang implementation.

## Important
You need to compile `ststd.erl` first before trying 
any tools as all of them will make heavy use of the 
abstract functions for i/o such as `getc` and `putc`.

Indentation is made by a code formatter before any commit, 
if any file is lacking indentation I'm sorry, please notify 
me or you can make a PR.

## File Structure
There are redundant files.
Some tools can be found on `stcN.erl` files, where `N` is 
the chapter number. Probably only chapters 1 and 2 will 
provide such a monolith, since later on chapter 2 we start 
to think about receive parameters and reusing tools with pipe, 
so most or all of the programs from late chapter 2 onwards 
will be executable escript files.
In those files there is a `-compile(export_all)` flag that 
helps in testing functions of different arities.

Using a function from `stcN` packages inside repl means
entering the Input yourself on arity 0 functions, or 
passing an Input on arity 1 functions.

Escript tools should accept both ststd:getc/1 for receiving 
data stream through pipes, and ststd:io/2 for files.

## Todo
- revise tools to see if they're all accepting pipes
- translit still needs to implement `^` (all but),
and `@` (escape characters).
Shouldn't be difficult but might need
another approach other than using maps as data structure,
it was a bad idea after all (the authors warned me!).
- print (3.6 Multi-stage Processing: Pipelines)
- makecopy (3.7 Creating Files Dinamically)
- archive (3.8 Putting it All Together: archive)
- sorting methods (the entire chapter 4)
