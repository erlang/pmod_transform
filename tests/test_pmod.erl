%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

-module(test_pmod).
-include_lib("eunit/include/eunit.hrl").

%% Test that it is safe to include the pmod parse transform for
%% plain (non-parameterized) modules.
-include_lib("pmod_transform/include/pmod.hrl").

basic_test() ->
    Prop1 = pmod_basic:new([{a,xb},{b,true},{c,false}]),
    Prop2 = pmod_basic:new([{y,zz}]),

    {a,xb} = Prop1:lookup(a),
    none = Prop1:lookup(glurf),
    false = Prop1:or_props([]),
    true = Prop1:or_props([b,c]),
    true = Prop1:or_props([b,d]),
    false = Prop1:or_props([d]),

    none = Prop2:lookup(kalle),
    {y,zz} = Prop2:lookup(y),
    {a,xb} = Prop1:lookup(a),

    Prop3 = Prop1:prepend({blurf,true}),
    {blurf,true} = Prop3:lookup(blurf),

    Prop4 = Prop3:append(42),
    {42,5} = Prop4:stupid_sum(),

    %% Some record guards.
    ok = Prop4:bar({s,0}),
    ok = Prop4:bar_bar({s,blurf}),
    error = Prop4:bar_bar({s,a,b}),
    error = Prop4:bar_bar([]),

    %% Record operations.
    Prop4:records(),

    %% Call from a fun.
    Fun = fun(Arg) -> Prop4:bar(Arg) end,
    ok = Fun({s,0}),

    [{y,[1,2]},{x,[5,19]}] = Prop4:collapse([{y,[2,1]},{x,[19,5]}]),

    P = pmod_basic:new(foo),
    [0,0,1,1,1,0,0,1] = P:bc1(),
    <<10:4>> = P:bc2().

%% Test extending a non-parameterized module.
extend_lists_test() ->
    [] = my_lists:empty_list(),
    {b,2} = my_lists:keyfind(b, 1, [{a,1},{b,2},{c,3}]),
    [1,2,3] = my_lists:usort([3,2,2,2,1]).

%% The following test cases are inspired by Richard Carlsson's
%% presentation at EUC 2007.
%%     http://www.erlang.se/euc/07/papers/1700Carlsson.pdf

alpha_test() ->
    M = alpha:new(x, y),
    {alpha,a,[x,y],[]} = M:a(),
    {alpha,b,[x,y],[1]} = M:b(1),
    {alpha,c,[x,y],[1,2]} = M:c(1, 2).

beta_test() ->
    M = beta:new(x, y),
    {beta,alpha,a,[x,y],[]} = M:a(),
    {beta,alpha,b,[x,y],[1]} = M:b(1),
    {alpha,c,[x,y],[1,2]} = M:c(1, 2).

gamma_test() ->
    M = gamma:new(z),
    {gamma,beta,a,[z,42],[]} = M:a(),
    {beta,alpha,b,[z,17],[1]} = M:b(1),
    {alpha,c,[z,17],[1,2]} = M:c(1, 2).

base_test() ->
    M = beta:new(x, y),
    {alpha,a,[x,y],[]} = M:call_base_a().

static_test() ->
    M = static_call:new(a, b),
    M:test().

self_extend_test() ->
    [{23,_,extends_self}] = fail_compile(bad_extend).

defining_instance_test() ->
    [{24,_,define_instance}] = fail_compile(defining_instance).

fail_compile(M) ->
    Filename = atom_to_list(M) ++ ".erl",
    error = compile:file(M),
    case compile:file(M, [return_errors]) of
	{error,[{Filename,Es}],[]} ->
	    Es
    end.
    
fun_test() ->
    M = fun_in_pmod:new(42),
    Add = M:add_fun(),
    44 = Add(2),
    Get = M:get_fun(),
    42 = Get().

-record(r, {a,b,c}).

record_index_test() ->
    {2,3,4} = T = {#r.a,#r.b,#r.c},
    record_index_match(T).

record_index_match({#r.a,#r.b,#r.c}) ->
    ok.
