-module(stc1).
-compile(export_all).

%% copy -- copy input to output
%%
%% We're not defining primitives like in the book,
%% We can't detect ENDFILE with io:get_line/1, and
%% it already receive only one line so no need to
%% detect ASCII NEWLINE as well.
%%
%% Since we can't detect ENDFILE, we can mimic ed(1)
%% behavior so that a "." in an empty line will quit.
%%
%% Correction: we can't detect ENDFILE in Erlang Shell,
%% but when running from terminal as an escript program,
%% you can pattern match Ctrl-D as eof.
copy() ->
Input = ststd:getc(),
copy(Input).

copy(eof) ->
{ok, eof};
copy(".\n") ->
{ok, eof};
copy(Input) ->
ststd:putc(Input),
copy().

%% charcount -- count characters in standard input
charcount() ->
Input = ststd:getf(),
charcount(Input, 0).

charcount(eof, Acc) ->
ststd:putf("~w~n", [Acc]);
charcount([], Acc) ->
charcount(eof, Acc);
charcount([_|T], Acc) ->
charcount(T, Acc + 1).

%% linecount -- count lines in standard input
%%
%% Since Erlang "String" is an array of numbers,
%% We can detect "\n" as ASCII 10.
linecount() ->
Input = ststd:getf(),
linecount(Input, 0).

linecount(eof, Acc) ->
ststd:putf("~w~n", [Acc]);
linecount([], Acc) ->
linecount(eof, Acc);
linecount([10|T], Acc) ->
linecount(T, Acc + 1);
linecount([_|T], Acc) ->
linecount(T, Acc).

%% wordcount -- count words in standard input
%%
%% ASCII Blank = 32, Newline = 10, Tab = 9.
%%
%% BUGS
%% If Ctrl-D during a word, will not count such a word.
wordcount() ->
Input = ststd:getf(),
wordcount(Input, false, 0).

wordcount(eof, Acc) ->
ststd:putf("~w~n", [Acc]).

wordcount([], _, Acc) ->
wordcount(eof, Acc);
wordcount([H|T], inword, Acc) ->
case lists:member(H, [9, 10, 32]) of
  true -> wordcount(T, false, Acc + 1);
  false -> wordcount(T, inword, Acc)
end;
wordcount([H|T], _, Acc) ->
case lists:member(H, [9, 10, 32]) of
  true -> wordcount(T, false, Acc);
  false -> wordcount(T, inword, Acc)
end.

%% putwc -- special output for wc
%%
%% BUGS
%% It doesn't print the name of the file as of yet.
putwc(ArgList) ->
ststd:putf(" ~w ~w ~w~n", ArgList).

%% wc -- count words, lines and characters
%%
%% BUGS
%% If Ctrl-D during a word, will not count word or curent line.
wc() ->
Input = ststd:getf(),
wc(Input, false, 0, 0, 0).

wc([], _, WC, LC, CC) ->
putwc([LC, WC, CC]);
wc([H|T], inword, WC, LC, CC) ->
  case lists:member(H, [9, 10, 32]) of
    true when H =:= 10 -> wc(T, false, WC+1, LC+1, CC+1);
    true -> wc(T, false, WC+1, LC, CC+1);
    false -> wc(T, inword, WC, LC, CC+1)
end;
wc([H|T], _, WC, LC, CC) ->
  case lists:member(H, [9, 10, 32]) of
    true when H =:= 10 -> wc(T, false, WC, LC+1, CC+1);
    true -> wc(T, false, WC, LC, CC+1);
    false -> wc(T, inword, WC, LC, CC+1)
end.

%% settabs -- set initial tab stops
%%
%% We can't generate a list/array because we don't have a MAXLINE.
%% settabs returns the TabSpace value instead.
settabs() ->
TabSpace = 4,
TabSpace.

%% tabpos -- return true if col is a tab stop
%%
%% Doesn't make sense in using MAXLINE since Erlang handle
%% the EOF and buffer size for us, unless we're using
%% io:get_chars/2 because the second parameter of the said
%% function is exactly a limiter.
tabpos(Col) ->
TabSpace = settabs(),
TabSpace - Col rem TabSpace.

%% detab - convert tabs to blanks
%%
%% It doesn't work exactely as the Pascal version.
%% See settabs/0 and tappos/1 for details.
%%
%% BUGS
%% Special characters other than 9, 10, 32 might transform
%% the return string in numbers and screw everything.
detab() ->
Input = ststd:getf(),
detab(Input, 0, []).

detab([], _, Acc) ->
ststd:putf("~p~n", [Acc]);
detab([9|T], Col, Acc) ->
Feed = tabpos(Col),
Blanks = lists:map(fun(_) -> 32 end, lists:seq(1, Feed)),
detab(T, Col+Feed, lists:concat([Acc, Blanks]));
detab([H|T], Col, Acc) ->
detab(T, Col+1, lists:concat([Acc, [H]])).