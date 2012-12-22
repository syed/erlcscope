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
	Files = lists:sort(string:tokens(binary_to_list(Data), ?NEWLINE_SEP)),
	{ok, Db} = file:open(?OUTPUT_FILE,[write]),
	init_symbol_db(Db,0),
	lists:foreach(fun(Fname) -> 
		build_symbol_db_from_file(Db,Fname) end, 
	Files),
	write_symbol_trailer_to_db(Db, Files),
	file:close(Db),
	
	% put the correct trailer offset 
	{ok , Db2} = file:open(?OUTPUT_FILE, [read,write]),
	init_symbol_db(Db2, TrailerOffset),
	file:close(Db2).

%% ====================================================================
%% Parsing and processing functions
%% ====================================================================

build_symbol_db_from_file(Db, SrcFile) ->
	write_symbol_to_db(Db, ?FILE_MARK, SrcFile),
	
	{ok, ParseTree} = epp_dodger:parse_file(Fname),
	{ok, FileData} = file:read_file(Fname),
	Lines = string:tokens(binary_to_list(FileData), "\n"),
	traverse_tree([ParseTree], #state{fd=SrcFd,data=Lines}).

traverse_tree(TreeList, S=#state{}) ->
	lists:foreach( 
	fun(NodeList) ->
		lists:foreach(fun(Node) ->
			{NewNode, NewState} = case erl_syntax:type(Node) of
						  atom -> process_atom(Node, S);
						  %function -> process_function(Node);
						  _ -> {erl_syntax:subtrees(Node), S}
					  end,
					  traverse_tree(NewNode,NewState)
    	end, NodeList)
	end, TreeList).

process_function(Tree) ->
	[].

process_atom(Node) ->
	AtomName = erl_syntax:atom_name(Node,S=#state{}),
	LineNo = erl_syntax:get_pos(Node),
	Line = string:strip(lists:nth(S#state.line_no - 1, S#state.data)),
	% search pos in line
	Pos = string:str( string:sub_string(Line, S#state.pos), AtomName),
	NewState = write_symbol_to_db(S, ?SYMBOL_MARK, AtomName, LineNo, Pos)
	{[], NewState}.

%% ====================================================================
%% Functions for writing to the file
%% ====================================================================

init_symbol_db(Db, TrailerOffset ) ->
	{ok, Cwd} = file:get_cwd(),
	Header = lists:flatten(io_lib:format("cscope ~s ~s -c ~10..0b~n", 
									[?CSCOPE_VERSION, Cwd,TrailerOffset])),
	io:format(Db,"~s",[Header]).

write_symbol_to_db(Db, ?FILE_MARK, Fname,_LineNo , _Pos , _State) ->
	io:format(Db,"~s~s~n~n", [?FILE_MARK, Fname]);
	
write_symbol_to_db(Db, Type, Name) ->
	io:format(Db,"~s~s~n~n",[Type, Name]).
	

write_symbol_trailer_to_db(Db,Files) ->
   	io:format(Db, "~s~n", [?SYMBOL_END]),
	TrailerOffset = filelib:file_size(?OUTPUT_FILE),
	io:format(Db, "1~n.~n0~n~p~n~p~n", [length(Files), lists:flatlength(Files) + length(Files)]),
	lists:foreach(fun(Fname) -> io:format(Db,"~s~n",[Fname]) end, Files).