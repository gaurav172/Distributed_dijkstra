-module(dijkstra).

-export([init_dijkstra/7, proc_run/6, distribute_graph/6]).

-import(helpers,[make_displs/2, get_minimum_vert/2, get_minimum_vert/3, get_minimum_vert/4, get_proc_rank/2, get_proc_rank/3, get_rank/0, get_bounds/2, get_row/3, get_col/3, hello/1]).
-import(distributors,[send_data/4, send_to_neighbours/2, wait_for_response/3, wait_for_response/4, read_and_send/2]).
-include("macros.hrl").


spawner(SysProps, Displs, Rank) ->
    spawn(dijkstra, proc_run, [Rank, helpers:get_bounds(Displs, Rank), #{}, SysProps, Displs, [self()]]).

reduce_task(ProcProps, ProcData, LocalData, SysProps, SourceProps, Pids)->
    {StartRow, EndRow} = ProcProps,
    ProcInfo = {ProcData, ProcProps},

    {_, _, Edges} = SourceProps,
    
    UpdateProcData = relax_edges(
                        SourceProps,
                        ProcInfo,
                        Edges
                    ),
    
    {Dist, Id} = get_minimum_vert(
                    UpdateProcData,
                    StartRow 
                ),

    distributors:send_to_neighbours(
            Pids,
            {
                reduction, 
                self(),
                {Dist, Id, maps:get(Id, LocalData, [])}   
            }
        ),

    receive
        {reduction, MinVertexProps} ->
            reduce_task(
                ProcProps,
                UpdateProcData,
                LocalData,
                SysProps,
                MinVertexProps,
                Pids
            );
        stop ->
            distributors:send_to_neighbours(
                Pids,
                {
                    result, 
                    self(),
                    lists:zip(lists:seq(StartRow, EndRow), element(1, UpdateProcData))     
                }
            ),
            ok
    end.
    

map_task(ProcProps, ProcData, LocalData, SysProps, SourceProps, Pids, Displs) ->
    % Get the vertices for current process
    % Accumulator just takes the minimum value of two Vars
    {StartRow, EndRow} = ProcProps,
    ProcInfo = {ProcData, ProcProps},
    Accumulator = fun(X, Y) ->
                case element(1, X) < element(1, Y) of
                    true ->
                        X;
                    false ->
                        Y
                end
            end, 
    
    {_, _, Edges} = SourceProps,
    
    UpdateProcData = relax_edges(
                        SourceProps,
                        ProcInfo,
                        Edges
                    ),

    % Get minimum vertex from each process
    {Dist, Id} = helpers:get_minimum_vert(
                    UpdateProcData,
                    StartRow
                ),
    
    MinVertexProps = distributors:wait_for_response(
                Pids,
                reduction,
                Accumulator,
                {Dist, Id, maps:get(Id, LocalData, [])}
            ),
        
    case element(1, MinVertexProps) of
        ?Inf ->
            distributors:send_to_neighbours(
                Pids, 
                stop
            ),
            Result = distributors:wait_for_response(
                Pids,
                result,
                fun(X, Y) ->
                    lists:append(X, Y)
                end,
                lists:zip(lists:seq(StartRow, EndRow), element(1, UpdateProcData))
            ),
            helpers:hello(["result", Result]),
            Result;
        _ ->
            {DistMin, IdMin, EdgeData} = MinVertexProps,
            {Data, NextEdges} = lists:split(lists:nth(1,Displs), EdgeData), 
            distributors:send_data(
                Pids,
                element(2, lists:split(1, Displs)),
                lists:nth(1,Displs)+1,
                {DistMin, IdMin, NextEdges}
            ),
            map_task(
                ProcProps,
                UpdateProcData,
                LocalData,
                SysProps,
                {DistMin, IdMin, Data},
                Pids,
                Displs
            )
    end.


relax_edges(SourceProps, ProcInfo, Edges) ->
    {ProcData, ProcProps} = ProcInfo,
    {StartRow, EndRow} = ProcProps,
    SourceId = element(2, SourceProps),
    LocalVis = element(2, ProcData),
    { 
        relax_edges(Edges, element(1, ProcData), 1, element(1, SourceProps)),
        if
            (StartRow =< SourceId) and (SourceId =< EndRow) ->
                A = lists:sublist(LocalVis, 1, SourceId - StartRow),
                B = lists:sublist(LocalVis, SourceId - StartRow + 2, EndRow- SourceId),
                lists:append([A, [1], B]);
            true ->
                element(2, ProcData)
        end
    }.

relax_edges(_, [], _, _) -> [];
relax_edges([Edge | RestEdges], [H|T], Index, BaseDist) ->
    case BaseDist + Edge < H of
        true -> 
            lists:append([BaseDist + Edge], relax_edges(RestEdges, T, Index+1, BaseDist));
        false ->
            lists:append([H], relax_edges(RestEdges, T, Index+1, BaseDist))
    end.



proc_run(Rank, ProcProps, LocalData, SysProps, Displs, Pids) ->
    receive
        {input, Data, CurRow} ->

            % Takes input Data and adds it to the LocalData List
            proc_run(
                Rank,
                ProcProps,
                maps:put(CurRow, Data, LocalData),
                SysProps,
                Displs,  
                Pids
            );
        {init, SourceProps} ->

            % Starts the algorithm 
            dijkstra:init_dijkstra(
                Rank,
                ProcProps,
                LocalData,
                SysProps,
                Displs, 
                SourceProps, 
                Pids
            ),
            ok;
        stop ->
            ok
    end.


init_dijkstra(Rank, ProcProps, LocalData, SysProps, Displs, SourceProps, Pids) ->
    ProcProps = helpers:get_bounds(Displs, Rank),
    % Get the Start and End vertices of this process
    {StartRow, EndRow} = ProcProps,
    {_, Source, _} = SourceProps,
    % Initialize the distance array by infinity for all vertices except for source.
    LocalDist = case { StartRow =< Source , Source =< EndRow } of
                    {true, true} ->
                        lists:append([lists:duplicate(Source-StartRow, ?Inf), [0], lists:duplicate(EndRow-Source, ?Inf)]);
                    {_, _} ->
                        lists:duplicate(EndRow-StartRow+1, ?Inf)
                end,
    
    LocalVis = lists:duplicate(EndRow-StartRow+1, 0),
    % ProcData will store the distance array and the visited Node
    ProcData = {LocalDist, LocalVis},
    % Node 1 will be the driving force and will send the source node to every vertex.
    case Rank of
        1 ->
            map_task(
                ProcProps,
                ProcData,
                LocalData,
                SysProps,
                SourceProps,
                Pids,
                Displs
            );

        _ ->         
            reduce_task(
                    ProcProps,
                    ProcData,
                    LocalData,
                    SysProps,
                    SourceProps,
                    Pids
            )
    end.


distribute_graph(Device, SysProps, Displs, CurRow, CurIndex, CurPid) ->
    % EndRow of a process can be found by its displacement no,
    EndRow = lists:nth(CurIndex, Displs),
    if 
        % This condition means that we have completed all N elements
        CurRow > element(1, SysProps) ->
            [];
        % Current Row belongs to the Process curPid so get its data
        % and send to curPid 
        CurRow =< EndRow ->
            distributors:read_and_send(Device, CurRow, CurPid),
            distribute_graph(
                Device, 
                SysProps, 
                Displs, 
                CurRow + 1, 
                CurIndex,
                CurPid
            );
        true ->
            Pid = spawner(SysProps, Displs, CurIndex+1),
            lists:append(
                [Pid],
                distribute_graph(
                    Device, 
                    SysProps, 
                    Displs, 
                    CurRow, 
                    CurIndex+1,
                    Pid
                ))
    end.