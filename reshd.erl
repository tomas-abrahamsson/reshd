%% ``The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved via the world wide web at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id: reshd.erl,v 1.1 2001-04-12 14:24:54 tab Exp $
%%
-module(shell).

-export([start/1,server/1,evaluator/3,local_func/4]).

-define(LINEMAX, 30).

start(IoPid) ->
    code:ensure_loaded(user_default),
    spawn(shell, server, [IoPid]).

server(IoPid) ->
    %% Our spawner has fixed the process groups.
    Bs = erl_eval:new_bindings(),
    process_flag(trap_exit, true),
    fwrite(IoPid, "Eshell V~s~n", [erlang:info(version)]),
    server_loop(0, start_eval(Bs, []), Bs, []).

server_loop(N0, Eval_0, Bs0, Ds0) ->
    N = N0 + 1,
    {Res, Eval0} = get_command(IoPid, prompt(N), Eval_0, Bs0, Ds0),
    case Res of 
	{ok,Es0,EndLine} ->			%Commands
	    case expand_hist(Es0, N) of
		{ok,Es} ->
		    {V,Eval,Bs,Ds} = shell_cmd(Es, Eval0, Bs0, Ds0, N),
		    del_cmd(N - 20),
		    add_cmd(N, Es, V),
		    server_loop(N, Eval, Bs, Ds);
		{error,E} ->
		    io:fwrite("** ~s **\n", [E]),
		    server_loop(N0, Eval0, Bs0, Ds0)
	    end;
	{error,{Line,Mod,What},EndLine} ->
	    io:fwrite("** ~w: ~s **\n", [Line,apply(Mod,format_error,[What])]),
	    server_loop(N0, Eval0, Bs0, Ds0);
	{error,terminated} ->			%Io process terminated
	    exit(Eval0, kill),
	    terminated;
	{error,interrupted} ->			%Io process interrupted us
	    exit(Eval0, kill),
	    {_,Eval,_,_} = shell_rep(Eval0, Bs0, Ds0),
	    server_loop(N0, Eval, Bs0, Ds0);
	{eof,EndLine} ->
	    io:fwrite("** Terminating erlang **\n"),
	    halt();
	eof ->
	    io:fwrite("** Terminating erlang **\n"),
	    halt()
    end.

get_command(IoPid, Prompt, Eval, Bs, Ds) ->
    Parse = fun() -> exit(io:parse_erl_exprs(IoPid, Prompt)) end,
    Pid = spawn_link(erlang, apply, [Parse, []]),
    get_command1(Pid, Eval, Bs, Ds).

get_command1(Pid, Eval, Bs, Ds) ->
    receive
	{'EXIT', Pid, Res} ->
	    {Res, Eval};
	{'EXIT', Eval, Reason} ->
	    io:fwrite("** exited: ~P **\n", [Reason, ?LINEMAX]),
	    get_command1(Pid, start_eval(Bs, Ds), Bs, Ds)
    end.

prompt(N) ->
    case is_alive() of
	true -> {format,"(~s)~w> ",[node(),N]};
	false -> {format,"~w> ",[N]}
    end.

%% expand_hist(Expressions, CommandNumber)
%%  Preprocess the expression list replacing all history list commands
%%  with their expansions.

expand_hist(Es, C) ->
    catch {ok,expand_exprs(Es, C)}.

expand_exprs([E|Es], C) ->
    [expand_expr(E, C)|expand_exprs(Es, C)];
expand_exprs([], C) ->
    [].

expand_expr({cons,L,H,T}, C) ->
    {cons,L,expand_expr(H, C),expand_expr(T, C)};
expand_expr({lc,L,E,Qs}, C) ->
    {lc,L,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({tuple,L,Elts}, C) ->
    {tuple,L,expand_exprs(Elts, C)};
expand_expr({record_index,L,Name,F}, C) ->
    {record_index,L,Name,expand_expr(F, C)};
expand_expr({record,L,Name,Is}, C) ->
    {record,L,Name,expand_fields(Is, C)};
expand_expr({record_field,L,R,Name,F}, C) ->
    {record_field,L,expand_expr(R, C),Name,expand_expr(F, C)};
expand_expr({record,L,R,Name,Ups}, C) ->
    {record,L,expand_expr(R, C),Name,expand_fields(Ups, C)};
expand_expr({record_field,L,R,F}, C) ->		%This is really illegal!
    {record_field,L,expand_expr(R, C),expand_expr(F, C)};
expand_expr({block,L,Es}, C) ->
    {block,L,expand_exprs(Es, C)};
expand_expr({'if',L,Cs}, C) ->
    {'if',L,expand_cs(Cs, C)};
expand_expr({'case',L,E,Cs}, C) ->
    {'case',L,expand_expr(E, C),expand_cs(Cs, C)};
expand_expr({'receive',L,Cs}, C) ->
    {'receive',L,expand_cs(Cs, C)};
expand_expr({'receive',L,Cs,To,ToEs}, C) ->
    {'receive',L,expand_cs(Cs, C), expand_expr(To, C), expand_exprs(ToEs, C)};
expand_expr({call,L,{atom,_,e},[N]}, C) ->
    case get_cmd(N, C) of
	{[Ce],V} ->
	    Ce;
	{Ces,V} ->
	    {block,L,Ces};
	undefined ->
	    no_command(N)
    end;
expand_expr({call,L,{atom,_,v},[N]}, C) ->
    case get_cmd(N, C) of
	{Ces,V} ->
	    {value,L,V};
	undefined ->
	    no_command(N)
    end;
expand_expr({call,L,F,Args}, C) ->
    {call,L,expand_expr(F, C),expand_exprs(Args, C)};
expand_expr({'catch',L,E}, C) ->
    {'catch',L,expand_expr(E, C)};
expand_expr({match,L,Lhs,Rhs}, C) ->
    {match,L,Lhs,expand_expr(Rhs, C)};
expand_expr({op,L,Op,Arg}, C) ->
    {op,L,Op,expand_expr(Arg, C)};
expand_expr({op,L,Op,Larg,Rarg}, C) ->
    {op,L,Op,expand_expr(Larg, C),expand_expr(Rarg, C)};
expand_expr({remote,L,M,F}, C) ->
    {remote,L,expand_expr(M, C),expand_expr(F, C)};
expand_expr(E, C) ->				%All constants
    E.

expand_cs([{clause,L,P,G,B}|Cs], C) ->
    [{clause,L,P,G,expand_exprs(B, C)}|expand_cs(Cs, C)];
expand_cs([], C) ->
    [].

expand_fields([{record_field,L,F,V}|Fs], C) ->
    [{record_field,L,expand_expr(F, C),expand_expr(V, C)}|
     expand_fields(Fs, C)];
expand_fields([], C) -> [].

expand_quals([{generate,L,P,E}|Qs], C) ->
    [{generate,L,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([E|Qs], C) ->
    [expand_expr(E, C)|expand_quals(Qs, C)];
expand_quals([], C) -> [].

no_command(N) ->
    throw({error,io_lib:fwrite("~s: command not found", [erl_pp:expr(N)])}).

%% add_cmd(Number, Expressions, Value)
%% get_cmd(Number, CurrentCommand)
%% del_cmd(Number)

add_cmd(N, Es, V) ->
    put({command,N}, {Es, V}).

get_cmd(Num, C) ->
    case catch erl_eval:expr(Num, []) of
	{value,N,_} when N < 0 -> get({command,C+N});
	{value,N,_} -> get({command,N});
	Other -> undefined
    end.

del_cmd(N) ->
    erase({command,N}).

%% shell_cmd(Sequence, Evaluator, Bindings, Dictionary, CommandNumber)
%% shell_rep(Evaluator) ->
%%	{Value,Evaluator,Bindings,Dictionary}
%%  Send a command to the evaluator and wait for the reply. Start a new
%%  evaluator if necessary.

shell_cmd(Es, Eval, Bs, Ds, N) ->
    Eval ! {shell_cmd,self(),{eval,Es}},
    shell_rep(Eval, Bs, Ds).

shell_rep(Ev, Bs0, Ds0) ->
    receive
	{shell_rep,Ev,{value,V,Bs,Ds}} ->
	    io:fwrite("~P~n", [V,?LINEMAX]),
	    {V,Ev,Bs,Ds};
	{shell_req,Ev,get_cmd} ->
	    Ev ! {shell_rep,self(),get()},
	    shell_rep(Ev, Bs0, Ds0);
	{shell_req,Ev,exit} ->
	    Ev ! {shell_rep,self(),exit},
	    io:fwrite("** Terminating shell **\n", []),
	    exit(normal);
	{'EXIT',Ev,Reason} ->			%It has exited unnaturally
	    io:fwrite("** exited: ~P **\n", [Reason,?LINEMAX]),
	    {{'EXIT',Reason},start_eval(Bs0, Ds0), Bs0, Ds0};
	{'EXIT',Id,interrupt} ->		%Someone interrupted us
	    exit(Ev, kill),
	    shell_rep(Ev, Bs0, Ds0);
	{'EXIT',Id,R} ->
	    exit(Ev, R),
	    exit(R);
	Other ->				%Ignore everything else
	    shell_rep(Ev, Bs0, Ds0)
    end.

start_eval(Bs, Ds) ->
    spawn_link(shell, evaluator, [self(),Bs,Ds]).

%% evaluator(Shell, Bindings, ProcessDictionary)
%%  Evaluate expressions from the shell. Use the "old" variable bindings
%%  and ductionary.

evaluator(Shell, Bs, Ds) ->
    init_dict(Ds),
    eval_loop(Shell, Bs).

eval_loop(Shell, Bs0) ->
    receive
	{shell_cmd,Shell,{eval,Es}} ->
	    {value,V,Bs} = erl_eval:exprs(Es, Bs0,
					  {eval,{shell,local_func},[Shell]}),
	    Shell ! {shell_rep,self(),{value,V,Bs,get()}},
	    eval_loop(Shell, Bs)
    end.

init_dict([{K,V}|Ds]) ->
    put(K, V),
    init_dict(Ds);
init_dict([]) -> true.

%% local_func(Function, Args, Bindings, Shell) ->
%%	{value,Val,Bs}
%%  Evaluate local functions, including shell commands.

local_func(h, [], Bs, Shell) ->
    Cs = shell_req(Shell, get_cmd),
    {value,list_commands(lists:keysort(1, Cs)),Bs};
local_func(b, [], Bs, Shell) ->
    {value,list_bindings(erl_eval:bindings(Bs)),Bs};
local_func(f, [], Bs, Shell) ->
    {value,ok,[]};
local_func(f, [{var,_,Name}], Bs, Shell) ->
    {value,ok,erl_eval:del_binding(Name, Bs)};
local_func(f, [Other], Bs, Shell) ->
    exit({function_clause,{shell,f,1}});
local_func(exit, [], Bs, Shell) ->
    shell_req(Shell, exit),			%This terminates us
    exit(normal);
local_func(F, As0, Bs0, Shell) when atom(F) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{shell,local_func},[Shell]}),
    case erlang:function_exported(user_default, F, length(As)) of
	true ->
	    {value,apply(user_default, F, As),Bs};
	false ->
	    {value,apply(shell_default, F, As),Bs}
    end;
local_func(F, As0, Bs0, Shell) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{shell,local_func},[Shell]}),
    {value,apply(F, As),Bs}.

shell_req(Shell, Req) ->
    Shell ! {shell_req,self(),Req},
    receive
	{shell_rep,Shell,Rep} -> Rep
    end.

list_commands([{{command,N},{Es,V}}|Ds]) ->
    io:requests([{format,"~w: ~s~n",[N,erl_pp:exprs(Es)]},
		 {format,"-> ~P~n",[V,?LINEMAX]}]),
    list_commands(Ds);
list_commands([D|Ds]) ->
    list_commands(Ds);
list_commands([]) -> ok.

list_bindings([{Name,Val}|Bs]) ->
    io:fwrite("~s = ~P~n", [Name,Val,?LINEMAX]),
    list_bindings(Bs);
list_bindings([]) ->
    ok.
