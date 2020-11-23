-module(helpers).
-export([
    make_displs/2,
    get_minimum_vert/2,
    get_minimum_vert/3,
    get_minimum_vert/4,
    get_proc_rank/2,
    get_proc_rank/3,
    get_bounds/2,
    get_row/3,
    get_col/3,
    hello/1]).

-include("macros.hrl").

hello(Line)->
    io:fwrite("~p~n", [Line]).


make_displs(NumVertices, NumProcs) -> 
    PerProc = NumVertices div NumProcs,
    RemProc = NumVertices rem NumProcs,
    % Seq creates an array starting with PerProc and increment of PerProc
    % Length of Array would be NumProcs 

    A = lists:seq(PerProc + 1, (PerProc + 1)*RemProc, PerProc + 1),
    B = case A of
        [] ->
            lists:seq(PerProc, NumVertices, PerProc);
        _ ->
            lists:seq(lists:last(A) + PerProc, NumVertices, PerProc)
        end,

    Displs = lists:append(A, B),
    Displs.

get_minimum_vert({Dist, Visited}, BaseVert) ->
    get_minimum_vert(Dist, BaseVert, Visited).

get_minimum_vert(Dist, BaseVert, Visited) ->
    get_minimum_vert(Dist, 1, BaseVert, Visited).

get_minimum_vert([], _, _, _) ->
    {?Inf, -1};

get_minimum_vert([H | T], Index, BaseVert, [Vis | VisitedList]) ->
    RetList = get_minimum_vert(T, Index+1, BaseVert, VisitedList),
    case element(1, RetList) < H orelse Vis == 1 of 
        false ->
            {H, Index+BaseVert-1};
        true ->
            RetList
    end.


get_proc_rank(Displs, RowNumber)->
    get_proc_rank(Displs, 1, RowNumber).
get_proc_rank(NumVertices, NumProcs, Vertex) when is_list(NumVertices) == false ->
    get_proc_rank(make_displs(NumVertices, NumProcs), Vertex);  
get_proc_rank(Displs, Rank, RowNumber)  ->
   case lists:nth(Rank, Displs) < RowNumber of
        true ->
            get_proc_rank(Displs, Rank+1, RowNumber);
        false ->
            Rank
    end.

get_bounds(Displs, Rank) ->
    EndRow = lists:nth(Rank, Displs),
    StartRow = case Rank of
        1 ->
            1;
        _ ->
            lists:nth(Rank-1, Displs) + 1
        end,
    {
        StartRow,
        EndRow
    }.

get_row(Data, RowNumber, NumVertices) ->
    lists:sublist(Data, NumVertices*RowNumber+1, NumVertices).

get_col([], _, _) -> [];
get_col(Data, ColNumber, NumVertices) ->
    % Gets the edges belonging to ColNumber vertex in the Data
    {Row, RestData} = lists:split(NumVertices, Data),
    lists:append(
        [lists:nth(ColNumber, Row)],
        get_col(RestData, ColNumber, NumVertices)
    ).

