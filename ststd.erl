-module(ststd).
-compile(export_all).

%% getc -- get one character from standard input
%%
%% This will help, exactly like the book teaches, to
%% abstract the input interface.
%% If we're to use io:get_line/1, once we wanted to get
%% chars (io:get_chars/2) or to get from a file or something,
%% we would need to comeback changing all occurrences of
%% io:get_line/1 to the new desired function.
%% Here, we can change once, or even define different interfaces
%% thanks to Erlang arity system, and it will already work
%% as intended.
getc() ->
    io:get_line("").

%% putc -- put one character on standard output
%%
%% Exactely as in getc, we're creating an abstract interface
%% to help maintain and adapt our code later.
putc(C) ->
    io:put_chars(C).

%% getf -- keep reading from standard input until eof or `.\n`
getf() ->
    Input = getc(),
    getf(Input, "").

getf(eof, State) ->
    State;
getf(".\n", State) ->
    State;
getf(Input, State) ->
    NextInput = getc(),
    getf(NextInput, lists:concat([State, Input])).

%% putf - put formatted data on standard output
putf(Format, Data) ->
    io:format(Format, Data).
%% iotype -- abstract input type from File argument
%%
%% BUGS
%% If file (Name) doesn't exists, will fail and might scream.
io({input, file}, Name) ->
    case file:read_file(Name) of
        {ok, Bin} -> erlang:binary_to_list(Bin);
        {error, enoent} -> {error, enoent}
    end;
io({output, file}, Acc) ->
    Bin = erlang:list_to_binary(Acc),
    file:write_file("output", Bin).

io({output, file}, Filename, Acc) ->
    Bin = erlang:list_to_binary(Acc),
    file:write_file(Filename, Bin).
