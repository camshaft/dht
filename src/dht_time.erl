%%% @doc Module dht_time proxies Erlang time calls
%%%
%%% The purpose of this module is to provide a proxy to typical Erlang/OTP
%%% time calls. It allows us to mock time in the test cases and keeps the
%%% interface local. It can also become a backwards-compatibility layer
%%% for release 17 and earlier.
%%% @end
%%% @private
-module(dht_time).

-export([monotonic_time/0, convert_time_unit/3, time_offset/0]).
-export([send_after/3, read_timer/1, cancel_timer/1]).

-spec monotonic_time() -> integer().
monotonic_time() ->
    {Mega, Sec, Micro} = erlang:now(),
    Mega * 1000000 * 1000000 + Sec * 1000000 + Micro.

time_offset() ->
    erlang:time_offset().

send_after(Time, Target, Msg) ->
    erlang:send_after(Time, Target, Msg).

read_timer(TRef) ->
    erlang:read_timer(TRef).

cancel_timer(TRef) ->
    erlang:cancel_timer(TRef).

-spec convert_time_unit(integer(), erlang:time_unit(), erlang:time_unit()) -> integer().
convert_time_unit(T, native, milli_seconds) ->
    trunc(T div 1000);
convert_time_unit(T, milli_seconds, native) ->
    T * 1000;
convert_time_unit(T, From, To) ->
    io:format("~p ~p ~p~n", [T, From, To]),
    erlang:convert_time_unit(T, From, To).
