-module (nono).
-compile ([export_all]).
-on_load (nif_load/0).

nif_load() ->
    ok = erlang:load_nif("./nono", 0).

solve(Hor, Ver) ->
    Width  = length(Hor),
    Height = length(Ver),
    Field = field(Width, Height),
    solve({hor, false}, Hor, Ver, lists:seq(1, Width), lists:seq(1, Height), Field).

field(Width, Height) ->
    [ lists:duplicate(Width, $-) || _Y <- lists:seq(1, Height)].

transform(F) ->
    [H | _] = F,
    [ col(F, I) || I <- lists:seq(1, length(H))].

col(Field, N) ->
    [ lists:nth(N, R) || R <- Field ].

solve({hor, true}, _Hor, _Ver, _W, _H, F) ->
    F;
solve({hor, false}, Hor, Ver, W, H, F) ->
    NF = transform(F),
    SF = lists:map(fun(C) ->
        Line = lists:nth(C, NF),
        case string:str(Line, "-") of
            0 -> Line;
            _ -> solve_line(lists:nth(C, Hor), Line)
        end
    end, W),
    NS = SF =:= NF,
    solve({ver, NS}, Hor, Ver, W, H, transform(SF));
solve({ver, M1}, Hor, Ver, W, H, F) ->
    SF = lists:map(fun(C) ->
        Line = lists:nth(C, F),
        case string:str(Line, "-") of
            0 -> Line;
            _ -> solve_line(lists:nth(C, Ver), Line)
        end
    end, H),
    NS = SF =:= F andalso M1 =:= true,
    [ io:format("~s~n", [I]) || I <- SF],
    % io:format("~n M1 = ~p  NS = ~p~n", [M1, NS]),
    io:format("~n~n~n"),
    solve({hor, NS}, Hor, Ver, W, H, SF).

solve_line(N, Line) ->
    solve_line(N, min_length(N), Line, string:len(Line)).

solve_line(N, ML, _Line, LL) when ML > LL ->
    erlang:error({baddata, N});
solve_line(N, ML, _Line, LL) when ML == LL ->
    string:substr(lists:flatten([ string:concat(lists:duplicate(X, $X), ".") || X <- N]), 1, ML);
solve_line(N, _ML, Line, LL) ->
    S = checkseqs(seqs(N, LL), Line),
    case S of
        [] -> Line;
        U  -> uniq(U)
    end.

min_length(N) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, N) + length(N) - 1.

seqs(N, L) ->
    M = L - lists:sum(N) + 1,
    C = length(N) + 1,
    S = lists:duplicate(C, lists:seq(1, M)),
    Sc = combos(S, M + 1),
    V = [ lists:flatten(lists:zipwith(
            fun(X, Y) -> X ++ Y end,
            [ lists:duplicate(Sp, $.) || Sp <- E],
            [ lists:duplicate(Z, $X) || Z <- N ] ++ [""]
        )) || E <- Sc],
    CV = [ string:substr(I, 2, length(I) - 2) || I <- V ],
    lists:filter(fun(X) -> length(X) == L end, CV).

combos(Lols, C) ->
    F = fun(_F, []) -> [[]]; (F, [H|T]) -> 
        [ check(X, Y, C) || X <- H, Y <- F(F, T)]
    end,
    R = F(F, Lols),
    lists:filter(fun(X) -> length(X) == length(hd(R)) end, R).

check(X, Y, C) ->
    XY = [X|Y],
    S = lists:sum(XY),
    case S > C of
        true -> Y;
        false -> XY
    end.

uniq(Lols) ->
    [H | T] = Lols,
    uniq(T, H).

uniq([], State) ->
    State;
uniq([CL | TL], State) ->
    NS = [
        case lists:nth(CC, CL) == lists:nth(CC, State) of
            true  -> lists:nth(CC, CL);
            false -> $-
        end
        || CC <- lists:seq(1, length(CL))],
    uniq(TL, NS).

checkseqs(Lols, P) ->
    NP = lists:flatten(["^"] ++ [
        case C of
            $- -> ".";
            $. -> "\\.";
            $X -> "X"
        end
        || C <- P ] ++ ["$"]),
    checkseqs(Lols, NP, []).

checkseqs([], _P, Acc) ->
    Acc;
checkseqs([CL | TL], P, Acc) ->
    case re:run(CL, P) of
        nomatch -> checkseqs(TL, P, Acc);
        _Match  -> checkseqs(TL, P, Acc ++ [CL])
    end.
