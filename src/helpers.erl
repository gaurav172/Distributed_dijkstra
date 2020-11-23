-module(helpers).
-export([
    make_displs/2,
    get_minimum_vert/2,
    get_minimum_vert/3,
    get_minimum_vert/4,
    get_proc_rank/2,
    get_proc_rank/3,
    % get_bounds/1,
    get_bounds/2,
    % get_pid/1,
    % get_rank/0,
    % get_rank/1,
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
    B = lists:seq(lists:last(A) + PerProc, NumVertices, PerProc),

    Displs = lists:append(A, B),
    Displs.
    % InitDispls = lists:droplast(lists:seq(PerProc, NumVertices, PerProc)),
    % lists:append(
    %     InitDispls,
    %     [
    %         case InitDispls of
    %             [] ->
    %                 PerProc+RemProc;
    %             _ ->
    %                 lists:last(InitDispls)+PerProc+RemProc
    %         end
    %     ]
    % ).

get_minimum_vert({Dist, Visited}, BaseVert) ->
    get_minimum_vert(Dist, BaseVert, Visited).

get_minimum_vert(Dist, BaseVert, Visited) ->
    get_minimum_vert(Dist, 1, BaseVert, Visited).

get_minimum_vert([], _, _, _) ->
    {?Inf, -1};

get_minimum_vert([H | T], Index, BaseVert, Visited) ->
    RetList = get_minimum_vert(T, Index+1, BaseVert, Visited),
    case element(1, RetList) < H orelse lists:member(Index+BaseVert-1, Visited) of 
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

% get_bounds(SysProps) -> 
    % get_bounds(SysProps, get_rank()).
get_bounds(Displs, Rank) ->
    % hello(SysProps),
    EndRow = lists:nth(Rank, Displs),
    StartRow = case Rank of
        1 ->
            1;
        _ ->
            lists:nth(Rank-1, Displs) + 1
        end,
    
    % {NumVertices, NumProcs} = SysProps,
    % StartRow = (Rank-1)*(NumVertices div NumProcs)+1,
    % EndRow = case Rank of
    %     NumProcs ->
    %         Rank*(NumVertices div NumProcs) + NumVertices rem NumProcs;
    %     _ ->
    %         Rank*(NumVertices div NumProcs)
    %     end,
    {
        StartRow,
        EndRow
    }.


% get_pid(Rank) ->
%     [{_, Pid}] = ets:lookup(procTable, Rank),
%     Pid.
% get_rank() -> 
%     get_rank(self()).
% get_rank(Pid) ->
%     [{_, Rank}] = ets:lookup(idTable, Pid),
%     Rank.

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

