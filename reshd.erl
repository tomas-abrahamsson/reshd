%%%----------------------------------------------------------------------
%%% Purpose : Remote erlang shell daemon -- a telnet interface to the shell
%%% File    : reshd.erl
%%% Author  : Tomas Abrahamsson <tab@lysator.liu.se>
%%% Created : 12 Apr 2001 by Tomas Abrahamsson <tab@lysator.liu.se>
%%%
%%% COPYRIGHT
%%%
%%% These programs are released into the public domain.  You may do
%%% anything you like with them, including modifying them and selling
%%% the binaries without source for ridiculous amounts of money without
%%% saying who made them originally.
%%% 
%%% However, I would be happy if you release your works with complete
%%% source for free use.
%%%----------------------------------------------------------------------
-module(reshd).
-author('tab@lysator.liu.se').

%% API
-export([start_link/1, start_link/2, start_link/3]).
-export([start/1, start/2, start/3]).
-export([stop/1, stop/2]).
-export([build_regname/1, build_regname/2]).

-export([format_error/1]).

%% Useful when starting from the command line, e.g: -s reshd c_start 0
-export([c_start/1]).
-export([cr_start/1]).
-export([cpl_start/1]).

%% Imports (a few well known ones, for enhanced readability)
-import(lists, [map/2, reverse/1, reverse/2]).

-record(config, {ip, port, inet, %% IP|Host, TCPPort and inet|inet6
		 keepalive,      %% boolean() %% set tcp-option for clients
		 silent_cprompt, %% boolean() %% set shell option
		 enc		 %% Encoding: unicode | latin1(?)
		}).

%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% API
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------

%% ----------------------------------------------------------------------
%% start(PortNumber)                -> Result
%% start(IP, PortNumber)            -> Result
%% start(PortNumber, Opts)          -> Result
%% start(IP, PortNumber, Opts)      -> Result
%% start_link(PortNumber)           -> LinkResult
%% start_link(IP, PortNumber)       -> LinkResult
%% start_link(PortNumber, Opts)     -> LinkResult
%% start_link(IP, PortNumber, Opts) -> LinkResult
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   Opts = [Opt]
%%   Opt = {ip, IP} |
%%         inet | inet6 |             %% IPv4 or IPv6, default = inet (ie IPv4)
%%         keepalive |                %% enable keepalive, default = false
%%         {silent_cprompt, boolean}  %% default = false
%%   IP = any | Ipv4Address | Ipv6Address | string() | atom()
%%      Ipv4Address = {Byte, Byte, Byte, Byte}
%%      Ipv6Address = {UShort,UShort,UShort,UShort,UShort,UShort,UShort,UShort}
%%      Byte   = integer(0..255)
%%      UShort = integer(0..65535)
%%   Result = {ok, UsedPortNumber} | {error, Reason}
%%   LinkResult = {ok, ServerPid, UsedPortNumber} | {error, Reason}
%%
%% Start the reshd server to listen for connections on TCP/IP port PortNumber.
%%
%% The special port number 0 means "use any available TCP/IP port".
%% The port that is actually used is returned. If PortNumber != 0, then
%% UsedPortNumber == PortNumber.
%%
%% Optionally, an IP address to bind to can also be specified.
%% The default is that IP any, which means to bind to all ip addresses
%% on the machine.
%%
%% The process that listens for and handles incoming connections is
%% locally registred under the name reshd_<IP>_<UsedPortNumber>.
%% build_regname is used to build the name.
%%
%% Use the option {silent_cprompt,true} if you intend to connect using
%% a windows telnet client. This will turn off printing the
%% (continuation) prompt on every input keystroke.
%%
%% Use the option keepalive or {keepalive,true} e.g. if you clients
%% connect via a firewall that silently drops idle connections and you
%% don't want your connections to get dropped.
%% ----------------------------------------------------------------------
start_link(PortNumber) ->
    server_start_link(set_port(PortNumber, opts_to_config([]))).

start_link(IP, PortNumber) when is_integer(PortNumber) ->
    server_start_link(set_port(PortNumber, opts_to_config([{ip,IP}])));
start_link(PortNumber, Opts) when is_list(Opts) ->
    server_start_link(set_port(PortNumber, opts_to_config(Opts))).

start_link(IP, PortNumber, Opts) when is_list(Opts) ->
    server_start_link(set_port(PortNumber, opts_to_config([{ip,IP} | Opts]))).

start(PortNumber) ->
    server_start(set_port(PortNumber, opts_to_config([]))).

start(IP, PortNumber) when is_integer(PortNumber) ->
    server_start(set_port(PortNumber, opts_to_config([{ip,IP}])));
start(PortNumber, Opts) when is_list(Opts) ->
    server_start(set_port(PortNumber, opts_to_config(Opts))).
start(IP, PortNumber, Opts) when is_list(Opts) ->
    server_start(set_port(PortNumber, opts_to_config([{ip,IP} | Opts]))).


%% ----------------------------------------------------------------------
%% c_start(Args)   -> Result
%% cr_start(Args)  -> void()
%% cpl_start(Args) -> void()
%%   Result = See start/1,2,3
%%
%% Use c_start when starting silently from the command line,
%% like this:     erl ... -s reshd c_start 12345
%% or like this:  erl ... -s reshd c_start 127.0.0.1 12345
%%
%% The cr_start is like c_start, except it also prints the start
%% result using io:format. This is useful if you use 0 for port number
%% and want to know what port number was actually used, for example if
%% you log the stdout to a file.
%%
%% The cpl_start is just like cr_start, except it issues a progress
%% report with error_logger:info_report instead of io:format, for
%% cases when the stdout goes to /dev/null, but the error_logger is
%% directed to a file. See the documentation for the error_logger
%% module adn the sasl application environment variables for more
%% info. Example (the quoting is unix /bin/sh quoting):
%%   erl -boot start_sasl \
%%       -sasl sasl_error_logger '{file,"/tmp/some-log"}' \
%%       ... \
%%       -s reshd cpl_start 127.0.0.1 0 \
%%       ...
%% ----------------------------------------------------------------------

c_start([PortNumberX]) ->
    PortNumber = if is_atom(PortNumberX) -> %% -s reshd c_start 34000
			 list_to_integer(atom_to_list(PortNumberX));
		    is_list(PortNumberX) -> %% -run reshd c_start 34000
			 list_to_integer(PortNumberX)
		 end,
    server_start(set_port(PortNumber, opts_to_config([])));
c_start([IP, PortNumberX]) ->
    PortNumber = if is_atom(PortNumberX) ->
			 list_to_integer(atom_to_list(PortNumberX));
		    is_list(PortNumberX) ->
			 list_to_integer(PortNumberX)
		 end,
    server_start(set_port(PortNumber, opts_to_config([{ip,IP}]))).

cr_start(Args) ->
    io:format("reshd -> ~p~n", [catch c_start(Args)]).

cpl_start(Args) ->
    error_logger:info_report(
      progress, [{reshd, {start_result, catch c_start(Args)}}]).


set_port(PortNumber, Config) ->
    Config#config{port = PortNumber}.

opts_to_config(Opts) ->
      #config{ip             = proplists:get_value(ip, Opts, any),
	      inet           = get_inet_opt(Opts),
	      silent_cprompt = proplists:get_value(silent_cprompt, Opts, false),
	      keepalive      = proplists:get_value(keepalive, Opts, false),
	      enc            = proplists:get_value(enc,Opts,get_default_enc())}.

get_inet_opt([inet | _])  -> inet;
get_inet_opt([inet6 | _]) -> inet6;
get_inet_opt([_ | T])     -> get_inet_opt(T);
get_inet_opt([])          -> inet.

get_default_enc() ->
    SysVsnStr = erlang:system_info(version),
    Vsn = map(fun list_to_integer/1, string:tokens(SysVsnStr, ".")),
    if Vsn >= [5,7] -> unicode;
       true         -> latin1
    end.
	    

%% ----------------------------------------------------------------------
%% stop(PortNumber) -> void()
%% stop(IP, PortNumber) -> void()
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   IP = any | {Byte,Byte,Byte,Byte}
%%   Byte = integer(0..255)
%% 
%% Stops the reshd server and any open connections associated to it. 
%% ----------------------------------------------------------------------
stop(PortNumber) ->
    stop(any, PortNumber).
stop(IP, PortNumber) ->
    server_stop(IP, PortNumber).


%% ----------------------------------------------------------------------
%% build_regname(PortNumber) -> atom()
%% build_regname(IP, PortNumber) -> atom()
%%   Portnumber = UsedPortNumber = integer(0..65535)
%%   IP = any | {Byte,Byte,Byte,Byte}
%%   Byte = integer(0..255)
%% 
%% Build a name under which the reshd server may be registered.
%% ----------------------------------------------------------------------
build_regname(PortNumber) ->
    build_regname(any, PortNumber).

build_regname(IP, PortNumber) ->
    list_to_atom(ff("~s_~s_~p", [?MODULE, hregname(IP), PortNumber])).

hregname(IP) when is_tuple(IP) -> %% IPv4 | IPv6
    string:join(map(fun integer_to_list/1, tuple_to_list(IP)), ".");
hregname(IP) when is_atom(IP) ->
    atom_to_list(IP);
hregname(IP) when is_list(IP) ->
    IP.

%% ----------------------------------------------------------------------
%% format_error(Error) -> string()
%%    Error = {error, Reason} | Reason
%% ----------------------------------------------------------------------
format_error({error, Reason}) ->
    format_error(Reason);
format_error({listen_failed, {IP, Inet, Port, Reason}}) ->
    ff("listen failed for port ~p on ~p ~p: ~s (~p)",
       [Port, Inet, IP, inet:format_error(Reason), Reason]);
format_error({ip_addr_lookup_failed, {HostOrAddr, Inet, Reason}}) ->
    ff("failed to resolve ~p host or address ~p: ~s (~p)",
       [Inet, HostOrAddr, inet:format_error(Reason), Reason]);
format_error(Err) ->
    ff("unknown error ~p", [Err]).



%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% Internal functions: the server part
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
server_start_link(#config{ip = IP} = Config) ->
    M = self(),
    Server = proc_lib:spawn_link(fun() -> server_init(M, Config) end),
    receive
	{ok, UsedPortNumber} ->
	    RegName = build_regname(IP, UsedPortNumber),
	    register(RegName, Server),
	    {ok, Server, UsedPortNumber};
	{error, {Symptom, Diagnostics}} ->
	    {error, {Symptom, Diagnostics}}
    end.


server_start(#config{ip = IP} = Config) ->
    M = self(),
    Server = proc_lib:spawn(fun() -> server_init(M, Config) end),
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
	    Mref = erlang:monitor(process,Pid),
	    Pid ! stop,
	    receive
		{'EXIT', Pid, _What} ->
		    receive
			{'DOWN', Mref, _, _, _} -> true
		    after 0 -> true
		    end,
		    ok;
		{'DOWN', Mref, _, _, _} ->
		    receive
			{'EXIT', Pid, _What} -> true
		    after 0 -> true
		    end,
		    ok
	    end
    end.

server_init(From, #config{ip   = IP,
			  inet = Inet,
			  port = Port} = Config) ->
    case config_to_gen_tcp_opt(Config) of
	{ok, GenTCPIPOpts} ->
	    ListenOpts = [list,
			  {packet, 0},
			  {active, true},		% this is the default
			  {nodelay, true},
			  {reuseaddr, true}] ++ GenTCPIPOpts,
	    case gen_tcp:listen(Port, ListenOpts) of
		{ok, ServerSocket} ->
		    {ok, UsedPort} = inet:port(ServerSocket),
		    From ! {ok, UsedPort},
		    process_flag(trap_exit, true),
		    server_loop(From, ServerSocket, Config);
		{error, Reason} ->
		    From ! {error, {listen_failed, {IP, Inet, Port, Reason}}}
	    end;
	{error, _Reason} = Err ->
	    From ! Err
    end.


config_to_gen_tcp_opt(#config{ip = any, inet = Inet}) ->
    {ok, [Inet]};
config_to_gen_tcp_opt(#config{ip = IP, inet = Inet}) when is_tuple(IP) ->
    {ok, [{ip, IP}, Inet]};
config_to_gen_tcp_opt(#config{ip = HostOrAddr, inet = Inet}) ->
    case inet:getaddr(HostOrAddr, Inet) of
	{ok, IP} ->
	    {ok, [{ip, IP}, Inet]};
	{error, Reason} ->
	    {error, {ip_addr_lookup_failed, {HostOrAddr, Inet, Reason}}}
    end.


server_loop(From, ServerSocket, Config) ->
    server_loop(From, ServerSocket, Config, []).

server_loop(From, ServerSocket, Config, Clients) ->
    case gen_tcp:accept(ServerSocket, 250) of
	{ok, ClientSocket} ->
	    ClientHandler = clienthandler_start(From, self(), ClientSocket,
						Config),
	    gen_tcp:controlling_process(ClientSocket, ClientHandler),
	    server_loop(From, ServerSocket, Config, [ClientHandler | Clients]);
	{error, timeout} ->
	    %% Check for signals now and then
	    receive
		stop ->
		    lists:foreach(fun(Client) -> Client ! stop end, Clients),
		    done;
		{client_stop, Client} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, Config, RemainingClients);
		{'EXIT', Client, _Reason} ->
		    RemainingClients = [C || C <- Clients, C /= Client],
		    server_loop(From, ServerSocket, Config, RemainingClients);
		Unexpected ->
		    loginfo("~p:server_loop: unexpected message:~p",
			    [?MODULE, Unexpected]),
		    server_loop(From, ServerSocket, Config, Clients)
	    after 0 ->
		    server_loop(From, ServerSocket, Config, Clients)
	    end;
	{error, Reason} ->
	    logerror("~p:server_loop: Error: accepting on ~p: ~p.",
		     [?MODULE, ServerSocket, Reason])
    end.


%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
%% The client handler part -- handles a user of the reshd.
%% ----------------------------------------------------------------------
%% ----------------------------------------------------------------------
clienthandler_start(From, Server, ClientSocket, Config) ->
    proc_lib:spawn_link(
      fun() -> clienthandler_init(From, Server, ClientSocket, Config) end).

-record(io_request,
	{
	  prompt, enc,
	  mod, fn, args,
	  from, reply_as
	 }).
	  
-record(env,
	{shell,
	 server,
	 csock,
	 config
	}).


clienthandler_init(_From, Server, ClientSocket,
		   #config{keepalive = KeepAlive} = Config) ->
    %% To protect against stupid stupid stupid routing or firewall or
    %% other network equipment that is set to disconnect after being
    %% idle a certain amount of time.
    inet:setopts(ClientSocket, [{keepalive,KeepAlive}]),

    %% Announce ourself as group leader.
    %% This causes all calls to io:format(...) and such alike
    %% to send their output to us.
    group_leader(self(), self()),

    %% Next, start the shell
    %% and link to it, so we know when it exits.
    process_flag(trap_exit, true),
    Shell = shell:start(true),
    link(Shell),

    Env = #env{shell  = Shell,
	       server = Server,
	       csock  = ClientSocket,
	       config = Config},

    %% Go ahead and take care of user input!
    case catch clienthandler_loop(idle, Env) of
	{'EXIT', Reason} ->
	    %% This is not a very good way of relaying a crash, but it
	    %% is the best we know
	    exit(Shell, kill),
	    exit(Reason);
	_ ->
	    exit(Shell, kill)
    end.
    

clienthandler_loop(State, Env) ->
    #env{shell  = Shell,
	 server = Server,
	 csock  = ClientSocket} = Env,
    receive
	{tcp, _Socket, Input} ->
	    NativeInput = nl_network_to_native(Input),
	    case handle_input(State, NativeInput, Env) of
		{ok, NewState, NewEnv} ->
		    clienthandler_loop(NewState, NewEnv);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{tcp_closed, _Socket} ->
	    Server ! {client_stop, self()},
	    done;

	{tcp_error, _Socket, _Reason} ->
	    Server ! {client_stop, self()},
	    done;

	stop ->
	    gen_tcp:close(ClientSocket),
	    done;

	{io_request, From, ReplyAs, Req} ->
	    case handle_io_request(State, From, ReplyAs, Req, Env) of
		{ok, NewState, NewEnv} ->
		    clienthandler_loop(NewState, NewEnv);
		close ->
		    gen_tcp:close(ClientSocket)
	    end;

	{'EXIT', Shell, normal} ->
	    gen_tcp:close(ClientSocket);

	{'EXIT', Shell, _OtherReason} ->
	    gen_tcp:close(ClientSocket);

	_Other ->
	    clienthandler_loop(State, Env)
    end.


%% Returns:
%%   {ok, NewState} |
%%   close
handle_input(State, Input, Env) ->
    #env{csock  = ClientSocket,
	 config = Config} = Env,
    case State of
	idle ->
	    {ok, {pending_input, Input}, Env};
	{pending_input, PendingInput} ->
	    NewInput = PendingInput ++ Input,
	    {ok, {pending_input, NewInput}, Env};
	{pending_request, Cont, [FirstReq | RestReqs] = Requests} ->
	    #io_request{prompt = Prompt,
			enc = Enc,
			mod = Mod,
			fn = Fun,
			args = Args} = FirstReq,
	    case apply(Mod, Fun, [Cont, Input | Args]) of
		{more, NewCont} ->
		    possibly_print_cprompt(ClientSocket, Prompt, Enc, Config),
		    {ok, {pending_request, NewCont, Requests}, Env};
		{done, Result, []} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    io_reply(From, ReplyAs, Result),
		    case RestReqs of
			[] ->
			    {ok, idle, Env};
			[#io_request{prompt = NextPrompt, enc = Enc2} | _] ->
			    possibly_print_cprompt(ClientSocket, NextPrompt,
						   Enc2, Config),
			    InitCont = init_cont(),
			    {ok, {pending_request, InitCont, RestReqs}, Env}
		    end;
		{done, Result, RestChars} ->
		    #io_request{from = From,
				reply_as = ReplyAs} = FirstReq,
		    io_reply(From, ReplyAs, Result),
		    case length(RestReqs) of
			0 ->
			    {ok, {pending_input, RestChars}, Env};
			_N ->
			    InitCont = init_cont(),
			    TmpState = {pending_request, InitCont, RestReqs},
			    handle_input(RestChars, TmpState, Env)
		    end
	    end
    end.

%% Returns:
%%   {ok, NewState, NewEnv} |
%%   close
handle_io_request(State, From, ReplyAs, IoRequest, Env) ->
    #env{csock  = ClientSocket,
	 config = Config} = Env,
    case IoRequest of
	{put_chars, Mod, Fun, Args} ->
	    do_put_chars(ClientSocket, latin1, Mod, Fun, Args, Config),
	    io_reply(From, ReplyAs, ok),
	    {ok, State, Env};

	{put_chars, Enc, Mod, Fun, Args} ->
	    do_put_chars(ClientSocket, Enc, Mod, Fun, Args, Config),
	    io_reply(From, ReplyAs, ok),
	    {ok, State, Env};

	{put_chars, Txt} ->
	    NWText = nl_native_to_network(convert_encoding(Txt,latin1,Config)),
	    gen_tcp:send(ClientSocket, NWText),
	    io_reply(From, ReplyAs, ok),
	    {ok, State, Env};

	{put_chars, Enc, Txt} ->
	    NWText = nl_native_to_network(convert_encoding(Txt, Enc, Config)),
	    gen_tcp:send(ClientSocket, NWText),
	    io_reply(From, ReplyAs, ok),
	    {ok, State, Env};

	{get_until, Prompt, Mod, Fun, Args} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = latin1,
				 mod = Mod,
				 fn = Fun,
				 args = Args,
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	{get_until, Enc, Prompt, Mod, Fun, Args} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = Enc,
				 mod = Mod,
				 fn = Fun,
				 args = Args,
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	{get_geometry, _} ->
	    io_reply(From, ReplyAs, {error,enotsup}),
	    {ok, State, Env};

	{setopts,NewOpts} ->
	    {Result, NewEnv} = do_setopts(NewOpts, Env),
	    io_reply(From, ReplyAs, Result),
	    {ok, State, NewEnv};

	{requests, IoReqests} ->
	    handle_io_requests(State, From, ReplyAs, IoReqests, Env);

	{get_password, _Prompt} ->
	    io_reply(From, ReplyAs, {error,enotsup}),
	    {ok, State, Env};

	{get_password, _Enc, _Prompt} ->
	    io_reply(From, ReplyAs, {error,enotsup}),
	    {ok, State, Env};
	    
	{get_line, Prompt} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = latin1,
				 mod = io_lib,
				 fn = collect_line,
				 args = [],
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	{get_line, Enc, Prompt} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = Enc,
				 mod = io_lib,
				 fn = collect_line,
				 args = [],
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	{get_chars, Prompt, N} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = latin1,
				 mod = io_lib,
				 fn = collect_chars,
				 args = [N],
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	{get_chars, Enc, Prompt, N} ->
	    NewReq = #io_request{prompt = Prompt,
				 enc = Enc,
				 mod = io_lib,
				 fn = collect_line,
				 args = [N],
				 from = From,
				 reply_as = ReplyAs},
	    do_get_until(NewReq, State, Env);

	UnexpectedIORequest ->
	    loginfo("~p:handle_io_request: Unexpected IORequest:~p~n",
		    [?MODULE, UnexpectedIORequest]),
	    io_reply(From, ReplyAs, {error,enotsup}),
	    {ok, State, Env}
    end.
    

do_put_chars(ClientSocket, InEnc, Mod, Fun, Args, Config) ->
    Text = case catch apply(Mod, Fun, Args) of
	       {'EXIT', _Reason} -> "";
	       Txt -> Txt
	   end,
    EncodedText = convert_encoding(Text, InEnc, Config),
    NWText = nl_native_to_network(EncodedText),
    gen_tcp:send(ClientSocket, NWText).

do_get_until(NewReq, State, Env) ->
    #env{csock  = ClientSocket,
	 config = Config} = Env,
    #io_request{prompt = Prompt} = NewReq,
    case State of
	{pending_request, Cont, PendingReqs} ->
	    NewState = {pending_request, Cont, PendingReqs++[NewReq]},
	    {ok, NewState, Env};

	idle ->
	    print_prompt(ClientSocket, Prompt, latin1, Config),
	    InitContinuation = init_cont(),
	    NewState = {pending_request, InitContinuation, [NewReq]},
	    {ok, NewState, Env};

	{pending_input, Input} ->
	    InitContinuation = init_cont(),
	    TmpState = {pending_request, InitContinuation, [NewReq]},
	    handle_input(TmpState, Input, Env)
    end.



convert_encoding(Text, latin1, #config{enc=latin1}) -> %% possibly R12B
    iolist_to_binary(Text);
convert_encoding(Text, InEnc, #config{enc=OutEnc}) ->
    case unicode:characters_to_binary(Text, InEnc, OutEnc) of
	Binary when is_binary(Binary) -> Binary;
	_                             -> <<>> %% What should we do??
    end.

do_setopts([{encoding, Enc} | Rest], Env = #env{config = Config}) ->
    NewConfig = Config#config{enc = Enc},
    NewEnv = Env#env{config = NewConfig},
    do_setopts(Rest, NewEnv);
do_setopts([_ | _], Env) ->
    {{error, enotsup}, Env};
do_setopts([], Env) ->
    {ok, Env}.

handle_io_requests(State0, From, ReplyAs, [LastIoReq], Env) ->
    handle_io_request(State0, From, ReplyAs, LastIoReq, Env);
handle_io_requests(State0, From, ReplyAs, [IoReq|Rest], Env)->
    case handle_io_request(State0, none, ReplyAs, IoReq, Env) of
	{ok, State1, Env1} ->
	    handle_io_requests(State1, From,ReplyAs,Rest, Env1);
	close ->
	    close
    end;
handle_io_requests(State, _From, _ReplyAs, [], Env) ->
    {ok, State, Env}.


init_cont() ->
    [].

io_reply(none, _ReplyAs, _Result) ->
    ok;
io_reply(From, ReplyAs, Result) ->
    From ! {io_reply, ReplyAs, Result}.

possibly_print_cprompt(ClientSocket, Prompt, InEnc, Config) ->
    if Config#config.silent_cprompt =:= false ->
	    print_prompt(ClientSocket, Prompt, InEnc, Config);
       Config#config.silent_cprompt =:= true ->
	    ok
    end.

print_prompt(ClientSocket, Prompt, InEnc, Config) ->
    PromptText = prompt_term_to_text(Prompt),
    NWPromptText = nl_native_to_network(
		     convert_encoding(PromptText, InEnc, Config)),
    gen_tcp:send(ClientSocket, NWPromptText).


prompt_term_to_text(Prompt) ->
    %% io_lib:format is present in R12 and later, it seems.
    case catch io_lib:format_prompt(Prompt) of
	{'EXIT', {undef, _}} -> print_prompt_text_fallback(Prompt);
	S when is_list(S)    -> S;
	B when is_binary(B)  -> B
    end.

print_prompt_text_fallback(Prompt) -> %% R11 or earlier.
    case Prompt of
	TxtAtom when is_atom(TxtAtom) ->
	    io_lib:format('~s', [TxtAtom]);
	{IoFun, PromptFmtStr, PromptArgs} ->
	    case catch io_lib:IoFun(PromptFmtStr, PromptArgs) of
		{'EXIT',_} -> "???";
		T -> T
	    end;
	{IoFun, PromptFmtStr} ->
	    case catch io_lib:IoFun(PromptFmtStr, []) of
		{'EXIT',_} -> "???";
		T -> T
	    end;
	Term ->
	    io_lib:write(Term)
    end.


%% Convert network newline (cr,lf) to native (\n)
nl_network_to_native(Input) ->
    nl_network_to_native(Input, "").

nl_network_to_native("\r\n" ++ Rest, Acc) ->
    nl_network_to_native(Rest, [$\n | Acc]);
nl_network_to_native([C | Rest], Acc) ->
    nl_network_to_native(Rest, [C | Acc]);
nl_network_to_native("", Acc) ->
    reverse(Acc).


%% Convert native newline \n to network (cr,lf)
nl_native_to_network(<<$\n, Rest/binary>>) ->
    <<$\r, $\n, (nl_native_to_network(Rest))/binary>>;
nl_native_to_network(<<C, Rest/binary>>) ->
    <<C,(nl_native_to_network(Rest))/binary>>;
nl_native_to_network(<<>>) ->
    <<>>.


%%%% ---------------------------------------------------------------------
%%%% Auxiliary routines follow
%%%% ---------------------------------------------------------------------

loginfo(FmtStr, Args) ->
    %% FIXME: Invent a way to log info.
    %% Can't use the error_log module since someone may
    %% add a log handler that does io:format. Then there
    %% will be a deadlock, I think, if this is function
    %% is called from within code that handles the client.
    Txt = fmt(FmtStr, Args),
    error_logger:info_msg("~s", [Txt]),
    fixme.
logerror(FmtStr, Args) ->
    %% See loginfo/2.
    Txt = fmt(FmtStr, Args),
    error_logger:error_msg("~s", [Txt]),
    fixme.

fmt(FmtStr, Args) ->
    case catch io_lib:format(FmtStr, Args) of
	{'EXIT', _Reason} ->
	    string_flatten(io_lib:format("Badly formatted text: ~p, ~p~n",
					[FmtStr, Args]));
	DeepText ->
	    string_flatten(DeepText)
    end.

ff(F, A) -> string_flatten(f(F, A)).

f(F, A) -> io_lib:format(F, A).

string_flatten(IoList) ->
    binary_to_list(iolist_to_binary(IoList)).
