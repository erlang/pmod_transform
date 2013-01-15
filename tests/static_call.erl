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
-include_lib("pmod_transform/include/pmod.hrl").

-module(static_call, [X,Y]).
-export([test/0,vars/0]).

test() ->
    Info = module_info(),
    true = is_list(Info),
    CInfo = module_info(compile),
    true = is_list(CInfo),

    M0 = new(x, y),
    {x,y} = M0:vars(),

    M1 = instance(a, b),
    {a,b} = M1:vars().

vars() ->
    {X,Y}.
