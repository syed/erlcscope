%% @author Syed Ahmed
%% @doc Program to erlang files and build a cscope database.
-module(erlcscope).
-include("erlcscope.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([main/0]).

main()->
	
	{ok, Data} = file:read_file(?INPUT_FILE),
	Files = lists:sort(string:tokens(binary_to_list(Data), "\n")),
	{ok, Db} = file:open(?OUTPUT_FILE,[write]),
	init_symbol_db(Db,0),
	lists:foreach(fun(Fname) -> build_symbol_db_from_file(Db,Fname) end, 
				   Files),
	io:format(Db, "~s~n", [?SYMBOL_END]),
	TrailerOffset = filelib:file_size(?OUTPUT_FILE),
	write_symbol_trailer(Db, Files),
	file:close(Db),
	
	% put the correct trailer offset 
	{ok , Db2} = file:open(?OUTPUT_FILE, [read,write]),
	init_symbol_db(Db2, TrailerOffset),
	file:close(Db2).


	
%% ====================================================================
%% Internal functions
%% ====================================================================


init_symbol_db(Db, TrailerOffset ) ->
	{ok, Cwd} = file:get_cwd(),
	Header = lists:flatten(io_lib:format("cscope ~s ~s -c ~10..0b~n", 
									[?CSCOPE_VERSION, Cwd,TrailerOffset])),
	io:format(Db,"~s",[Header]).
	
build_symbol_db_from_file(Db, Fname) ->
	io:format(Db,"~s~s~n~n", [?FILE_MARK, Fname]),
	{ok, Source} = file:read_file(Fname),
	Symbols = parse_and_build_crossref(Fname,binary_to_list(Source) ),
	io:format(Db, "~s", [Symbols]).

parse_and_build_crossref(Fname, Data) ->
	{ok, Forms} = epp_dodger:parse_file(Fname),
	Symbols = process_syntax_tree(Forms),
	[].

process_syntax_tree(Tree) ->
	lists:foreach(fun(T) ->
		Type = erlang:element(2, T),
		case Type of 
			function -> process_function(Tree);
			atom -> process_atom(Tree);
			_ -> []
		end
				  end, Tree).

process_function(Tree) ->
	[].

process_atom(Tree) ->
	[].

write_symbol_trailer(Db,Files) ->
	io:format(Db, "1~n.~n0~n~p~n~p~n", [length(Files), lists:flatlength(Files) + length(Files)]),
	lists:foreach(fun(Fname) -> io:format(Db,"~s~n",[Fname]) end, Files).