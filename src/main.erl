-module(main).

-export([start/0]).

-import(distributors, [register_proc/2, read_int_line/1]).
-import(helpers, [make_displs/2, hello/1]).
-import(dijkstra, [init_dijkstra/6, proc_run/5, distribute_graph/6]).
-include("macros.hrl").

% Compile all the files
compile_files(Files) ->
    lists:foreach(fun(X) ->
        compile:file(X)
    end,
    Files
    ).

start() ->
    compile_files([
        distributors,
        helpers,
        dijkstra 
    ]),
    
% Open the test file to read
    {ok, Device} = file:open("./test2.txt", [read]),
% Get first line of input and print it.
    [NumVertices, NumProcs, Source] = distributors:read_int_line(Device),
    
    helpers:hello([NumVertices, NumProcs, Source]),
% Stores number of vertices and number of processes 
    SysProps = {NumVertices, NumProcs},
% Stores displacement, displs[i] = num of vertices in 1..i 
    % Displs = lists:append(helpers:make_displs(NumVertices, NumProcs), [ ?Inf ] ),
    Displs = helpers:make_displs(NumVertices, NumProcs),
    helpers:hello(["Nodes Per Processes", Displs]),
    % Takes the input Graph. Each Row represents some data element.
    GetData = fun F(Data, CurRow, EndRow) ->
                if 
                    CurRow > EndRow ->
                        Data;
                    true ->
                        Row = distributors:read_int_line(Device),
                        % F(lists:append(Data, Row), CurRow+1, EndRow)
                        F(maps:put(CurRow, Row, Data), CurRow + 1, EndRow)
                end
            end,
    

    LocalData = GetData(#{}, 1, lists:nth(1, Displs)),
    Pids = dijkstra:distribute_graph(Device, SysProps, Displs, lists:nth(1, Displs)+1, 1, self()),
    distributors:send_data_init(
        Pids,
        element(2, lists:split(1, Displs)),
        lists:nth(1,Displs)+1,
        {0, Source, element(2,lists:split(lists:nth(1,Displs), maps:get(Source, LocalData)))}
    ),
    {Time, _} = timer:tc(dijkstra,init_dijkstra,[
        1,
        helpers:get_bounds(Displs, 1),
        LocalData,
        SysProps,
        Displs,
        {0, Source, maps:get(Source, LocalData)}, 
        Pids
    ]),
   helpers:hello([Time/1000000]),
   file:close(Device).