%%%----------------------------------------------------------------------
%%% File    : telnetif.erl
%%% Author  : Tomas Abrahamsson <tab@lysator.liu.se>
%%% Purpose : Telnet interface
%%% Created : 26 Sep 2000 by Tomas Abrahamsson <tab@lysator.liu.se>
%%%----------------------------------------------------------------------

-module(telnetif).
-author('tab@lysator.liu.se').
-vsn('$Revision: 1.1 $'). % '
-rcs('$Id: telnetif.erl,v 1.1 2001-04-12 14:24:54 tab Exp $').	% '

%%-compile(export_all).
%%-export([Function/Arity, ...]).

%% API
-export([start/1, start/2]).
-export([stop/1, stop/2]).
-export([broadcast/2, broadcast/3]).


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

broadcast(PortNumber, Msg) ->
    broadcast(any, PortNumber, Msg).
broadcast(IP, PortNumber, Msg) ->
    server_broadcast(IP, PortNumber, Msg).


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


server_broadcast(IP, PortNumber, Msg) ->
    RegName = build_regname(IP, PortNumber),
    case whereis(RegName) of
	undefined ->
	    do_nothing;
	Pid ->
	    Pid ! {broadcast, Msg}
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
	    logfile:log("~p: IP lookup failed for ~p: ~p. Binding to any ip.",
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
		{broadcast, Msg} ->
		    lists:foreach(fun(Client) ->
					  Client ! {broadcast, Msg}
				  end,
				  Clients),
		    server_loop(From, ServerSocket, Clients);
		{client_stop, Client} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		{'EXIT', Client, Reason} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, RemainingClients);
		Unexpected ->
		    logfile:log("~p:server_loop: unexpected message:~p",
				[?MODULE, Unexpected]),
		    server_loop(From, ServerSocket, Clients)
	    after 0 ->
		    server_loop(From, ServerSocket, Clients)
	    end;
	{error, Reason} ->
	    logfile:log("~p:server_loop: Error: accepting on ~p: ~p.",
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

-record(state,
	{
	  %% Possible states:
	  %% idle                    -- no input from socket,
	  %%                            no request for input
	  %% {pending_input, Bytes}  -- input from socket, 
	  %%                            no request for input
	  %% {pending_request, C, R} -- no input from socket,
	  %%                            there is a request for input
	  %%				See the requests field for the requests
	  %% closed                  -- socket is closed
	  state = idle,

	  %% List of pending #io_request{}s
	  %%? requests = []
	 }).

-record(io_request,
	{
	  prompt,
	  mod, fn, args,
	  from, reply_as
	 }).
	  

clienthandler_init(From, Server, ClientSocket) ->
    State = #state{},
    clienthandler_loop(State, From, Server, ClientSocket).

clienthandler_loop(State, From, Server, ClientSocket) ->
    receive
	{tcp, _Socket, InData} ->
	    case handle_indata(State, InData) of
		{ok, NewState} ->
		    clienthandler_loop(NewState, From, Server, ClientSocket);
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

	{broadcast, Msg} ->
	    respond(ClientSocket, Msg),
	    clienthandler_loop(From, Server, ClientSocket, PendingData);

	{io_request, From, ReplyAs, IoRequest} ->
	    case handle_io_request(State, From, ReplyAs, IoRequest) of
		{send_reply, To, Me, Text, IoReply, NewState} ->
		    gen_tcp:send(ClientSocket, Text),
		    To ! {io_reply, Me, IoReply},
		    clienthandler_loop(NewState, From, Server, ClientSocket);
		{reply, To, Me, IoReply, NewState} ->
		    To ! {io_reply, Me, IoReply},
		    clienthandler_loop(NewState, From, Server, ClientSocket);
		{ok, NewState} ->
		    clienthandler_loop(NewState, From, Server, ClientSocket);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	Other ->
	    logfile:log("~p:clienthandler_loop: Other event?!: ~p~n", [Other]),
	    clienthandler_loop(From, Server, ClientSocket, PendingData)
    end.


handle_indata(State, InData) -> % {ok, NewState} | close
    case State of
	idle ->
	    {ok, {pending_input, InData}=NewState};
	{pending_input, PendingInput} ->
	    NewInput = PendingInput ++ InData,
	    {ok, {pending_input, NewInput}=NewState};
	{pending_request, Cont, [FirstReq | RestReqs] = Requests} ->
	    #io_request{mod = Mod,
			fn = Fun,
			args = Args,
			cont = Cont} = FirstReq,
	    case catch apply(Mod, Fun, [Cont, InData|Args]) of
		{more, NewCont} ->
		    NewFirstReq = FirstReq#io_request{cont = NewCont};
		{done, Result, RestChars} ->
		    fixme
	    end
    end.


%% Returns:
%%   {reply, To, ReplyAs, Reply, NewState} |
%%   {send_reply, To, ReplyAs, Text, Reply, NewState} |
%%   {send, To, ReplyAs, Reply, NewState} |
%%   {ok, NewState} |
%%   close
handle_io_request(State, From, ReplyAs, IoRequest) ->
    case Request of
	{put_chars, Mod, Fun, Args} ->
	    Text = case catch apply(Mod, Fun, Args) of
		      {'EXIT', Reason} -> "";
		      Txt -> Txt
		   end,
	    {send_reply, From, ReplyAs, Text, ok=Reply, State};
	{put_chars, Text} ->
	    {send_reply, , From, ReplyAs, Text, ok=Reply, State};
	{get_until, Prompt, Mod, Fun, Args} ->
	    NewReq = #io_request{prompt = Prompt,
				 fn = Fun,
				 args = Args,
				 from = From,
				 reply_as = ReplyAs},
	    case State of
		{pending_request, Cont, PendingReqs} ->
		    NewState = {pending_request, Cont, PendingReqs++[NewReq]},
		    {ok, NewState};

		idle ->
		    InitCont = init_cont(),
		    NewState = {pending_request, InitCont, [NewReq]},
		    {ok, NewState};
		{pending_input, Input} ->
		    fixme
	    end;
	UnexpectedIORequest ->
	    io:format("Unexpected IORequest:~p~n", [UnexpectedIORequest]),
	    {reply, From, ReplyAs, ok=Reply, State}
    end.
    



%% ----------------------------------------------------------------------
%% parse_data -- parse a chunk of data, if possible
%% Arguments:	Data = [char()]
%% Returns:	close_connection | incomplete |
%%		{complete, ParsedLines, LeftOverData}
%% ----------------------------------------------------------------------
parse_data([4 | OtherData]) ->			% on linux?
    close_connection;
parse_data([255 | OtherData]) ->		% on solaris? or my stty?
    close_connection;
parse_data(Data) ->
    case string:rstr(Data, "\r\n") of
	0 ->
	    incomplete;
	SubStrStartIndex ->
	    LinesPart = string:substr(Data, 1, SubStrStartIndex - 1),
	    LeftOverData = string:substr(Data, SubStrStartIndex + 2),
	    Lines = string:tokens(LinesPart, "\r\n"),
	    ParsedLines = parse_cmds(Lines),
	    {complete, ParsedLines, LeftOverData}
    end.

parse_cmds(Lines) ->
    lists:map(fun(Line) -> parse_cmd(Line) end, Lines).

parse_cmd(Line) ->
    case list_to_term(Line) of
	{ok, Term} ->
	    {cmd, Term};
	{error, ErrorInfo} ->
	    {badcommand, ErrorInfo}
    end.


%% ----------------------------------------------------------------------
%% handle_cmds -- handle a command
%% ----------------------------------------------------------------------
handle_cmds(ClientSocket, Messages) ->
    lists:foldl(fun(Message, close=Acc) ->
			close;
		   (Message, ok) ->
			% This one must return ok or close
			handle_cmd(ClientSocket, Message)
		end,
		ok,
		Messages).

handle_cmd(ClientSocket, close) ->
    close;
handle_cmd(ClientSocket, {cmd, {rpc_req, N, M, F, A}}) ->
    case catch apply(M, F, A) of
	{'EXIT', Reason} ->
	    respond(ClientSocket, {error, Reason});
	Result ->
	    respond(ClientSocket, {rpc_resp, N, Result})
    end,
    ok;
handle_cmd(ClientSocket, {cmd, {send_req, PidStr, Msg}}) when list(PidStr)->
    Pid = list_to_pid(PidStr),		%dirty, I know
    Pid ! Msg,
    ok;
handle_cmd(ClientSocket, {cmd, {send_req, PidAtom, Msg}}) when atom(PidAtom)->
    case whereis(PidAtom) of
	undefined ->
	    dont_send;
	Pid ->
	    Pid ! Msg
    end,
    ok;
handle_cmd(ClientSocket, {badcommand, ErrorInfo}) ->
    respond(ClientSocket, {error, {"bad command", ErrorInfo}}),
    ok;
handle_cmd(ClientSocket, Cmd) ->
    respond(ClientSocket, {error, {"bad unknown command", Cmd}}),
    ok.


%% ----------------------------------------------------------------------
%% respond -- send an answer to the client
%% ----------------------------------------------------------------------
respond(ClientSocket, Term) ->
    TermText = io_lib:print(Term, 1, 1000000, -1),
    gen_tcp:send(ClientSocket, TermText ++ "\r\n").


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% Auxiliary functions
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% list_to_term -- string() to term()
%% Returns:	{ok, Term} | {error, ErrorInfo}
%% ----------------------------------------------------------------------
list_to_term(StringNoDot) ->
    case erl_scan:string(StringNoDot ++ ".") of
	{ok, Tokens, EndLine} ->
	    %% This returns {ok, Term} | {error, ErrorInfo}
	    erl_parse:parse_term(Tokens);
	{error, ErrorInfo, EndLine} ->
	    {error, ErrorInfo}
    end.
