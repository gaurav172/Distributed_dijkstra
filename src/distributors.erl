-module(distributors).

-include("macros.hrl").
-export([
    read_int_line/1,
    send_data/4,
    send_data_init/4,
    send_to_neighbours/2, 
    wait_for_response/3,
    wait_for_response/4, 
    read_and_send/3]).

-import(helpers, [hello/1]).



% Reads a line, Trim will remove extra spaces from the line.
% Split converts it into a List of Space separated strings
% Check_inf function will then convert it to Integer and convert -1 to Infinity

read_int_line(Device) ->
    Row = file:read_line(Device),
    % Check if X is Infinity or not
    Check_inf = fun(X) -> 
            {Int, _ } = string:to_integer(X),
            case Int == -1 of
                true -> ?Inf;
                false -> Int
            end
        end,
    case Row of 

        eof ->
            [];
        {ok, Line} ->
            lists:map(
                 Check_inf,
                 string:split(string:trim(Line), " ", all)
                )
    end.

send_data_init([], _, _, _) -> ok;
send_data_init([Pid | Pids], [EndRow | Displs], CurRow , Msg) ->
    {Dist, Id, Edges} = Msg,
    {Data, NextEdges} = lists:split(EndRow - CurRow + 1, Edges),

    Pid ! {init, {Dist, Id, Data}},
    send_data_init(Pids, Displs, EndRow + 1, {Dist, Id, NextEdges}).

send_data([], _, _, _) -> ok;
send_data([Pid | Pids], [EndRow | Displs], CurRow , Msg) ->
    {Dist, Id, Edges} = Msg,
    {Data, NextEdges} = lists:split(EndRow - CurRow + 1, Edges),

    Pid ! {reduction, {Dist, Id, Data}},
    send_data(Pids, Displs, EndRow + 1, {Dist, Id, NextEdges}).


send_to_neighbours(Neighs, Msg) ->
    lists:foreach(
            fun(Pid) ->
                Pid ! Msg
            end,
            Neighs
        ).

wait_for_response(Pids, Response, Accumulator) -> 
    wait_for_response(Pids, Response, Accumulator, {?Inf, -1}).

wait_for_response([], _, _, Result) -> Result;
wait_for_response(Pids, Response, Accumulator, Result) ->
    receive
        {Response, Pid, Data} ->
            wait_for_response(
                lists:delete(Pid, Pids), 
                Response, 
                Accumulator, 
                Accumulator(Result, Data)
            )
    end.

read_and_send(Device, CurRow, Id) ->
    Row = read_int_line(Device),
    case Row of 
        [] -> 
            ok;
        [H|T] ->
            Msg = {input, Row, CurRow},
            % Sends Msg to process with PID id
            Id ! Msg,
            [H|T]
    end.

