%%%----------------------------------------------------------------------
%%% File    : reshd.erl
%%% Author  : Tomas Abrahamsson <tab@lysator.liu.se>
%%% Purpose : Telnet interface to the shell
%%% Created : 12 Apr 2001 by Tomas Abrahamsson <tab@lysator.liu.se>
%%%----------------------------------------------------------------------

-module(reshd).
-author('tab@lysator.liu.se').
-vsn('$Revision: 1.2 $'). % '
-rcs('$Id: reshd.erl,v 1.2 2001-04-18 10:54:06 tab Exp $').	% '

%%-compile(export_all).
%%-export([Function/Arity, ...]).

%% API
-export([start/1, start/2]).
-export([stop/1, stop/2]).


%% exports due to spawns
-export([server_init/3]).
-export([clienthandler_init/3]).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% The server part -- a server for listening to incoming socket
%%                    connections a clienthandler process is spawned
%%                    for every new connection.
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
start(PortNumber) ->
    start(any, PortNumber).
start(IP, PortNumber) ->
    server_start(IP, PortNumber).

stop(PortNumber) ->
    stop(any, PortNumber).
stop(IP, PortNumber) ->
    server_stop(IP, PortNumber).


server_start(IP, PortNumber) ->
    Server = spawn(?MODULE, server_init, [self(), IP, PortNumber]),
    receive
	{ok, UsedPortNumber} ->
	    RegName = build_regname(IP, UsedPortNumber),
	    register(RegName, Server),
	    {ok, UsedPortNumber};
	{error, {Symptom, Diagnostics}} ->
	    {error, {Symptom, Diagnostics}}
    end.


server_stop(IP, PortNumber) ->
    RegName = build_regname(IP, PortNumber),
    case whereis(RegName) of
	undefined ->
	    do_nothing;
	Pid ->
	    Pid ! stop
    end.


build_regname(any, PortNumber) ->
    Name = atom_to_list(?MODULE) ++ "_any_" ++ integer_to_list(PortNumber),
    list_to_atom(Name);
build_regname({IP1, IP2, IP3, IP4}, PortNumber) ->
    Name = atom_to_list(?MODULE) ++ "_" ++
	list_to_integer(IP1) ++ "_" ++
	list_to_integer(IP2) ++ "_" ++
	list_to_integer(IP3) ++ "_" ++
	list_to_integer(IP4) ++ "_" ++
	"_" ++ integer_to_list(PortNumber),
    list_to_atom(Name);
build_regname(HostNameOrIP, PortNumber) ->
    Name = atom_to_list(?MODULE) ++
	"_" ++ HostNameOrIP ++ "_" ++
	integer_to_list(PortNumber),
    list_to_atom(Name).


server_init(From, IP, PortNumber) ->
    IPOpt = ip_to_opt(IP),
    ListenOpts = [list,
		  {packet, 0},
		  {active, true},		% this is the default
		  {nodelay, true},
		  {reuseaddr, true}] ++ IPOpt,
    case gen_tcp:listen(PortNumber, ListenOpts) of
	{ok, ServerSocket} ->
	    {ok, UsedPortNumber} = inet:port(ServerSocket),
	    From ! {ok, UsedPortNumber},
	    process_flag(trap_exit, true),
	    server_loop(From, ServerSocket);
	{error, Reason} ->
	    From ! {error, {listen_failed, Reason}}
    end.


ip_to_opt(any) ->
    [];
ip_to_opt({IP1, IP2, IP3, IP4}=IPNumber) ->
    [{ip, IPNumber}];
ip_to_opt(HostNameOrIPAsString) ->
    case inet:getaddr(HostNameOrIPAsString, inet) of
	{ok, IPNumber} ->
	    [{ip, IPNumber}];
	{error, Error} ->
	    io:format("~p: IP lookup failed for ~p: ~p. Binding to any ip.",
		      [?MODULE,HostNameOrIPAsString, Error]),
	    []
    end.


server_loop(From, ServerSocket) ->
    server_loop(From, ServerSocket, []).

server_loop(From, ServerSocket, Clients) ->
    case gen_tcp:accept(ServerSocket, 250) of
	{ok, ClientSocket} ->
	    ClientHandler = clienthandler_start(From, self(), ClientSocket),
	    gen_tcp:controlling_process(ClientSocket, ClientHandler),
	    server_loop(From, ServerSocket, [ClientHandler | Clients]);
	{error, timeout} ->
	    %% Check for signals now and then
	    receive
		stop ->
		    lists:foreach(fun(Client) -> Client ! stop end, Clients),
		    done;
		{client_stop, Client} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		{'EXIT', Client, Reason} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		Unexpected ->
		    io:format("~p:server_loop: unexpected message:~p",
			      [?MODULE, Unexpected]),
		    server_loop(From, ServerSocket, Clients)
	    after 0 ->
		    server_loop(From, ServerSocket, Clients)
	    end;
	{error, Reason} ->
	    io:format("~p:server_loop: Error: accepting on ~p: ~p.",
		      [?MODULE, ServerSocket, Reason])
    end.


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% The client handler part -- handles a client:
%%                            * reads, parses and executes commands
%%			      * returns the result from the commands
%%			        to the client.
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
clienthandler_start(From, Server, ClientSocket) ->
    spawn_link(?MODULE, clienthandler_init, [From, Server, ClientSocket]).

-record(io_request,
	{
	  prompt,
	  mod, fn, args,
	  from, reply_as
	 }).
	  

clienthandler_init(From, Server, ClientSocket) ->
    %% Announce ourself as group leader.
    %% This causes all calls to io:format(...) and such alike
    %% to send their output to us.
    group_leader(self(), self()),

    %% Next, start the shell
    %% and link to it, so we know when it exits.
    process_flag(trap_exit, true),
    Reshd = shell:start(true),
    link(Reshd),

    %% Go ahead and take care of user input!
    R = (catch clienthandler_loop(idle, Reshd, Server, ClientSocket)),
    exit(Reshd, kill).

clienthandler_loop(State, Reshd, Server, ClientSocket) ->
    receive
	{tcp, _Socket, InData} ->
	    case handle_input(ClientSocket, State, InData) of
		{ok, NewState} ->
		    clienthandler_loop(NewState, Reshd, Server, ClientSocket);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{tcp_closed, Socket} ->
	    Server ! {client_stop, self()},
	    done;

	{tcp_error, Socket, Reason} ->
	    Server ! {client_stop, self()},
	    done;

	stop ->
	    gen_tcp:close(ClientSocket),
	    done;

	{io_request, From, ReplyAs, Req} ->
	    case handle_io_request(ClientSocket, State, From, ReplyAs, Req) of
		{ok, NewState} ->
		    clienthandler_loop(NewState, Reshd, Server, ClientSocket);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{'EXIT', Reshd, normal} ->
	    gen_tcp:close(ClientSocket);

	{'EXIT', Reshd, _OtherReason} ->
	    gen_tcp:close(ClientSocket);

	Other ->
	    clienthandler_loop(State, Reshd, Server, ClientSocket)
    end.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_input(ClientSocket, State, InData) ->
    case State of
	idle ->
	    {ok, {pending_input, InData}};
	{pending_input, PendingInput} ->
	    NewInput = PendingInput ++ InData,
	    {ok, {pending_input, NewInput}};
	{pending_request, Cont, [FirstReq | RestReqs] = Requests} ->
	    #io_request{prompt = Prompt,
			mod = Mod,
			fn = Fun,
			args = Args} = FirstReq,
	    case catch apply(Mod, Fun, [Cont, InData|Args]) of
		{more, NewCont} ->
		    PromptText = case Prompt of
				     {IoFun, PromptFmtStr, PromptArgs} ->
					 io_lib:IoFun(PromptFmtStr,PromptArgs);
				     {IoFun, PromptFmtStr} ->
					 io_lib:IoFun(PromptFmtStr, [])
				 end,
		    gen_tcp:send(ClientSocket, PromptText),
		    {ok, {pending_request, NewCont, Requests}};
		{done, Result, []} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    From ! {io_reply, ReplyAs, Result},
		    case length(RestReqs) of
			0 ->
			    {ok, idle};
			N ->
			    InitCont = init_cont(),
			    {ok, {pending_request, InitCont, RestReqs}}
		    end;
		{done, Result, RestChars} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    From ! {io_reply, ReplyAs, Result},
		    case length(RestReqs) of
			0 ->
			    {ok, {pending_input, RestChars}};
			N ->
			    InitCont = init_cont(),
			    TmpState = {pending_request, InitCont, RestReqs},
			    handle_input(ClientSocket, RestChars, TmpState)
		    end;
		Other ->
		    io:format("Unexpected result from call: ~p~n", [Other]),
		    close
	    end
    end.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_io_request(ClientSocket, State, From, ReplyAs, IoRequest) ->
    case IoRequest of
	{put_chars, Mod, Fun, Args} ->
	    Text = case catch apply(Mod, Fun, Args) of
		      {'EXIT', Reason} -> "";
		      Txt -> Txt
		   end,
	    gen_tcp:send(ClientSocket, Text),
	    From ! {io_reply, ReplyAs, ok},
	    {ok, State};

	{put_chars, Text} ->
	    gen_tcp:send(ClientSocket, Text),
	    From ! {io_reply, ReplyAs, ok},
	    {ok, State};

	{get_until, Prompt, Mod, Fun, Args} ->
	    PromptText = case Prompt of
			     {IoFun, PromptFmtStr, PromptArgs} ->
				 io_lib:IoFun(PromptFmtStr, PromptArgs);
			     {IoFun, PromptFmtStr} ->
				 io_lib:IoFun(PromptFmtStr, [])
			 end,
	    gen_tcp:send(ClientSocket, PromptText),
	    NewReq = #io_request{prompt = Prompt,
				 mod = Mod,
				 fn = Fun,
				 args = Args,
				 from = From,
				 reply_as = ReplyAs},
	    case State of
		{pending_request, Cont, PendingReqs} ->
		    NewState = {pending_request, Cont, PendingReqs++[NewReq]},
		    {ok, NewState};

		idle ->
		    InitContinuation = init_cont(),
		    NewState = {pending_request, InitContinuation, [NewReq]},
		    {ok, NewState};

		{pending_input, Input} ->
		    InitContinuation = init_cont(),
		    TmpState = {pending_request, InitContinuation, [NewReq]},
		    handle_input(ClientSocket, TmpState, Input)
	    end;

	UnexpectedIORequest ->
	    io:format("Unexpected IORequest:~p~n", [UnexpectedIORequest]),
	    From ! {io_reply, ReplyAs, ok},
	    {ok, State}
    end.
    

init_cont() ->
    [].
