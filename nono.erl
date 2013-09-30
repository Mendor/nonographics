-module (nono).
% -compile ([export_all]).
-export ([nif_load/0, solve/2, pp/1]).
-on_load (nif_load/0).

nif_load() ->
    ok = erlang:load_nif("./nono", 0).

%% Solve the puzzle!
%% Exported function. Put the numbers as horizontal and vertical data
%%   parameters; see README for the details.
solve(HorData, VerData) ->
    Width  = length(HorData),
    Height = length(VerData),
    Field  = field(Width, Height),
    solve({hor, false},
           HorData, VerData,
           lists:seq(1, Width), lists:seq(1, Height),
           Field).

%% Generate a field with required width and height.
field(Width, Height) ->
    [ lists:duplicate(Width, $-) || _Y <- lists:seq(1, Height) ].

%% Transform field rows to columns and columns to rows.
transform(Field) ->
    [Line | _] = Field,
    [ col(Field, I) || I <- lists:seq(1, length(Line)) ].

%% Get single column from the field.
col(Field, N) ->
    [ lists:nth(N, R) || R <- Field ].

%% Attempts to solve puzzle from the field data.
solve({hor, true}, _HorData, _VerData, _W, _H, Field) ->
    % If the last pair of vertical & horizontal passes finished without any
    % changes, just return current field state, nothin' to do here anymore.
    Field;
solve({hor, false}, HorData, VerData, Width, Height, Field) ->
    % Pass based on horizontal data (columns).
    % Transform field to make working with columns easier.
    TrField = transform(Field),
    SolvedField = solve_field(TrField, HorData, Width),
    Unsolvable = SolvedField =:= TrField,
    solve({ver, Unsolvable},
          HorData, VerData,
          Width, Height,
          transform(SolvedField)); % don't forget to transform the field back
solve({ver, UHor}, HorData, VerData, Width, Height, Field) ->
    % Pass based on vertical data (rows)
    SolvedField = solve_field(Field, VerData, Height),
    Unsolvable = SolvedField =:= Field andalso UHor =:= true,
    % pp(SolvedField),
    % io:format("~n~n~n"),
    solve({hor, Unsolvable},
          HorData, VerData,
          Width, Height,
          SolvedField).

%% Field solution iteration.
solve_field(Field, Data, Size) ->
    lists:map(fun(C) ->
        Line = lists:nth(C, Field),
        % If the current line is unsolved, try to improve the situation.
        case string:str(Line, "-") of
            0 -> Line;
            _ -> solve_line(lists:nth(C, Data), Line)
        end
    end, Size).

%% Line solution "API".
solve_line(Nums, LineData) ->
    solve_line(Nums, min_length(Nums), LineData, string:len(LineData)).

%% Line solution iteration.
solve_line(Nums, MinDataLen, _LD, LineLen) when MinDataLen > LineLen ->
    % If the minimal possible length based on the provided line data is larger
    % than puzzle's line length, there is an error in input data, definitely.
    erlang:error({baddata, Nums});
solve_line(Nums, MinDataLen, _LD, LineLen) when MinDataLen == LineLen ->
    % If there is only one solution for the line; useless hack.
    string:substr(lists:flatten(
        [ string:concat(lists:duplicate(X, $X), ".") || X <- Nums ]
    ), 1, MinDataLen);
solve_line(Nums, _MDL, LineData, LineLen) ->
    % All other cases.
    % TODO: write something there
    case check_seqs(seqs(Nums, LineLen), LineData) of
        [] -> LineData;
        U  -> common(U)
    end.

%% Find minimal possible length of the puzzle line based on it's arguments.
min_length(Nums) ->
    lists:foldl(fun(X, Sum) -> X + Sum end, 0, Nums) + length(Nums) - 1.

%% Generate all sequences satisfying line parameters.
seqs(_Nums, _LineLen) ->
    "You don't want to know how terrible is pure Erlang sequences generator. "
    ++ "Compile and load NIF to do it best.".

%% Find overlapping areas of provided potential solutions. "API" part.
common(Lols) ->
    [H | T] = Lols,
    common(T, H).

%% Find overlapping areas of provided potential solutions. Internal part.
common([], State) ->
    State;
common([CurrentList | OtherLists], State) ->
    NewState = [
        case lists:nth(Char, CurrentList) == lists:nth(Char, State) of
            true  -> lists:nth(Char, CurrentList);
            false -> $-
        end
        || Char <- lists:seq(1, length(CurrentList)) ],
    common(OtherLists, NewState).

%% Filter potential solutions which match unsolved line pattern. "API" part.
check_seqs(Lols, Pattern) ->
    % Just generate a regular expression from text pattern to do it.
    Regex = lists:flatten(["^"] ++ [
        case C of
            $- -> ".";
            $. -> "\\.";
            $X -> "X"
        end
        || C <- Pattern ] ++ ["$"]),
    check_seqs(Lols, Regex, []).

%% Filter potential solutions which match unsolved line pattern. Internal part.
check_seqs([], _Regex, Acc) ->
    Acc;
check_seqs([CurrentList | OtherLists], Regex, Acc) ->
    case re:run(CurrentList, Regex) of
        nomatch -> check_seqs(OtherLists, Regex, Acc);
        _Match  -> check_seqs(OtherLists, Regex, Acc ++ [CurrentList])
    end.

%% Pretty print multiple strings, and nothing else.
pp(Strings) ->
    [ io:format("~s~n", [S]) || S <- Strings ].
