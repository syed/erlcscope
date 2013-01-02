-module(simple_tcp).
-compile(export_all).

-define(
   install_help,
   [
    "  ts:install()           - Install TS with no Options.\n"
    "  ts:install([Options])  - Install TS with Options\n"
    "\n",
    "Installation options supported:\n",
    "  {longnames, true} - Use fully qualified hostnames\n",
    "  {verbose, Level}  - Sets verbosity level for TS output (0,1,2), 0 is\n"
    "                      quiet(default).\n"
    "  {crossroot, ErlTop}\n"
    "                    - Erlang root directory on build host, ~n"
    "                      normally same value as $ERL_TOP\n"
    "  {crossenv, [{Key,Val}]}\n"
    "                    - Environmentals used by test configure on build host\n"
    "  {crossflags, FlagsString}\n"
    "                    - Flags used by test configure on build host\n"
    "  {xcomp, XCompFile}\n"
    "                    - The xcomp file to use for cross compiling the~n"
    "                      testcases. Using this option will override any~n"
    "                      cross* configurations given to ts. Note that you~n"
    "                      have to have a correct ERL_TOP as well.~n"
   ]).

-define(TryAfter(Timeout), 
	{'EXIT',{timeout_value,_}} = (catch receive mission -> exit(impossible) after Timeout -> ok end),
	{'EXIT',{timeout_value,_}} = (catch receive after Timeout -> ok end),
	try_after(Timeout)).

-define(heap_binary_size, 64).

-define(try_match(E),
	catch ?MODULE:bar(),
	{'EXIT', {{badmatch, nomatch}, _}} = (catch E = id(nomatch))).



start_server(Port) ->
    Pid = spawn_link( fun() ->
        {ok, Listen} = gen_tcp:listen(Port, [binary,{active, false}]),
        spawn(fun()->acceptor(Listen) end),
        timer:sleep(infinity)
    end),
    {ok,Pid}.

acceptor(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> acceptor(ListenSocket) end),
    handle(Socket).

% Simple echo server

handle(Socket) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, <<"quit", _/binary>>} -> 
            gen_tcp:close(Socket);
        {tcp, Socket, Msg} ->
            gen_tcp:send(Socket, Msg),
            handle(Socket)
    end.


