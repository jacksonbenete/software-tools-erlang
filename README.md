# Software Tools in Erlang
This is an implementation of various Unix common tools, as taught on `Software Tools in Pascal` by Kernighan and Plauger. This is my interpretation of a Erlang implementation.

There are redundant files.
All the tools can be found on `stcn.erl` files, where `n` is the chapter number. So you can compile and call the stc1 module to have all chapter one tools in one place.
That's easier than having to compile each single tool separately.
In those files there is a `-compile(export_all)` flag that helps in testing functions of different arities.

Some files are written as escript instead, such as `echo` and `translit`. All files that are supposed to receive parameters from terminal will be written as escripts.

Possibly, each separeted tool from `stcn.erl` files will be provided as separeted files as well.
