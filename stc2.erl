-module(stc2).
-compile(export_all).

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

%% entab -- convert runs of blanks into tabs
%%
%% BUGS
%% Is (possibly) not an exact inverse of detab.
%% It doesn't convert a single blank to a tab if it occurs
%% at a tab stop. This behavior was considered a bug in Pascal,
%% here it could be considered a bug to don't have such a bug.
entab(Input) ->
entab(Input, 0, []).

entab([], _, Acc) ->
Acc;
%% Blank/Whitespace = ASCII 32
entab([32|T], Blanks, Acc) ->
TabSize = settabs(),
case Blanks =:= (TabSize - 1) of
  true -> entab(T, 0, lists:concat([Acc, [9]]));
  false -> entab(T, Blanks+1, Acc)
end;
entab([H|T], Blanks, Acc) ->
Seq = lists:seq(1, Blanks),
BlanksList = lists:map(fun(_) -> 32 end, Seq),
entab(T, 0, lists:concat([Acc, BlanksList, [H]])).

%% overstrike -- replace overstrikes by multiple lines
%%
%% BUGS
%% I'm not really sure what is an overstrike as I'm not used
%% to typewriters.
%% Also, Erlang inputs don't handle backspace \b or [8].
%% What overstrike is doing here is to mimick the output
%% from the book example, which is basically a underline.
overstrike(Input) ->
overstrike(Input, lists:concat([Input, "\n"])).

overstrike([], Acc) ->
Acc;
overstrike([H|T], Acc) ->
case H =:= 32 of
  true -> overstrike(T, lists:concat([Acc, [H]]));
  false -> overstrike(T, lists:concat([Acc, "_"]))
end.

%% compress -- compress standard input
%%
%% BUGS
%% The escape symbol should be passed down instead of written
%% directly on Pattern Matching.
%% The problem is that we can't call a function inside Pattern,
%% and I didn't wanted to increase the function arity only for
%% a constant value.
%%
%% Runs longer than 26 character aren't being broken into sev.
%% shorter ones, but this should be easy to correct.
compress(Input) ->
compress(Input, [], 0, []).

compress([], Char, Count, Acc) ->
StateList = putrep(Char, Count),
lists:concat([Acc, StateList]);
compress([126|T], Char, Count, Acc) ->
% ~ it's a run of itself of lenght 1
StateList = putrep(Char, Count),
compress(T, [], 0, lists:concat([Acc, StateList, [126, 65, 126]]));
compress([H|T], Char, Count, Acc) when H =/= Char ->
compress(T, H, 1, lists:concat([Acc, putrep(Char, Count)]));
compress([_|T], Char, Count, Acc) ->
compress(T, Char, Count+1, Acc).

%% putrep -- put out representations of run of n 'c's
%%
%% BUGS
%% This one isn't a exactly copy of the Pascal version.
%% This should use a constant instead of the escaping value 126.
putrep(Char, Count) ->
case Count >= thresh() of
  true -> [126, init_symbol() + Count, Char];
  false -> lists:map(fun(_) -> Char end, lists:seq(1, Count))
end.

%% init_symbol -- get the firs symbol for count repetitions
%%
%% This is a abstraction instead of using letter 'A' directly.
init_symbol() ->
Symbol = 64,  % ASCII 64 = @, next one is 'A'
Symbol.

%% thresh -- return the threshold for repetitions to compress
thresh() ->
Threshold = 4,
Threshold.

%% expand -- uncompress standard input
%%
%% We had to put guards ranging from 65 to 122, or else
%% the conversion starts to fail on lists:seq/2 logic.
expand(Input) ->
Base = init_symbol(),
expand(Input, Base, []).

expand([], _, Acc) ->
Acc;
expand([126, Count, Char|T], Base, Acc) when Count >= 65 andalso Count =< 122 ->
Finding = lists:map(fun(_) -> Char end, lists:seq(1, Count - Base)),
expand(T, Base, lists:concat([Acc, Finding]));
expand([H|T], Base, Acc) ->
expand(T, Base, lists:concat([Acc, [H]])).
