%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    cth_colorize.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-03-26
%%

-module(cth_colorize).

%% Callbacks
-export([id/1, init/2]).

-export([pre_init_per_suite/3,
         post_init_per_suite/4,
         pre_end_per_suite/3,
         post_end_per_suite/4]).

-export([pre_init_per_group/3,
         post_init_per_group/4,
         pre_end_per_group/3,
         post_end_per_group/4]).

-export([pre_init_per_testcase/3,
         post_init_per_testcase/4,
         pre_end_per_testcase/3,
         post_end_per_testcase/4]).

-export([on_tc_fail/3,
         on_tc_skip/3]).

-export([terminate/1]).

-record(state,{id,
               t0,
               t1,
               suite}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state. 
init(Id, _Opts) ->
    {ok, #state{id = Id}}.

%% @doc Called before init_per_suite is called. 
pre_init_per_suite(Suite,Config,State) ->
    {Config, State#state{suite=Suite}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite. 
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite. 
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each end_per_group. 
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group. 
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_testcase.
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State}.

%% Called after each init_per_testcase (immediately before the test case).
post_init_per_testcase(_TC,_Config,Return,State) ->
    {Return, State#state{t0=erlang:monotonic_time(micro_seconds)}}.

%% @doc Called before each end_per_testcase (immediately after the test case).
pre_end_per_testcase(_TC,Config,State) ->
    {Config, State#state{t1=erlang:monotonic_time(micro_seconds)}}.

%% @doc Called after each end_per_testcase.
%% (yellow)* (gray)Text(N*space)(blue)[ (boldred)ok (blue)]
post_end_per_testcase(TC,_Config,ok,#state{suite=Suite, t1=T1, t0=T0}=State) ->
    ok = format_status(Suite, TC, T1 - T0, " ~!GOK "),
    {ok, State};
post_end_per_testcase(_TC,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(TC, Reason, #state{suite=Suite, t1=T1, t0=T0}=State) ->
    ok = format_status(Suite, TC, T1 - T0, " ~!R!! "),
    ok = format_reason(Reason),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing.  
on_tc_skip(TC, _Reason, #state{suite=Suite, t1=T1, t0=T0}=State) ->
    ok = format_status(Suite, TC, T1 - T0, "~!mSKIP"),
    %ok = format_reason(_Reason),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State) ->
    ok.

%% format
format(Format,Args) ->
    io:format(user, Format, Args).

string(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

columns() ->
    case io:columns() of
        {ok,Cols} -> Cols;
        {error,enotsup} -> 80
    end.

format_status(Suite,TC,Duration,StatusStr) ->
    CaseStr = string("~w:~w", [Suite,TC]),
    TimeStr = string("~w us", [Duration]),
    N = length(CaseStr),
    W = columns() - N - 3 - 6 - 3,
    Color = " ~!y* ~!w~ts~!! ~~~wts ~!b["++ StatusStr ++"~!b]~!!",
    Format = lists:flatten(cf:format(Color,[CaseStr,W])),
    Str = string(Format,[TimeStr]),
    format("~ts~n",[Str]),
    ok.

format_reason({failed, {Error, Stack}}) ->
    StackFun = fun(_,_,_) -> false end,
    FormatFun = fun (Term, _) -> io_lib:format("~p", [Term]) end,
    format("Failed with: ~ts~n~ts~n", [io_lib:format("~p",[Error]),
                                       lib:format_stacktrace(1, Stack, StackFun,
                                                             FormatFun)]),
    ok;
format_reason(Reason) ->
    format("Failed with: ~p~n",[Reason]),
    ok.
