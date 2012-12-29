%% @author Syed Ahmed
%% @doc Program to parse erlang files and build a cscope database.
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
	write_symbol_trailer_to_db(Db, Files).

%% ====================================================================
%% Parsing and processing functions
%% ====================================================================

build_symbol_db_from_file(Db, SrcFile) ->
	{ok, FileData} = file:read_file(SrcFile),
	io:format("file ~p~n",[SrcFile]),
	Lines = split_data_to_lines(binary_to_list(FileData)),
	State = #state{db=Db,data=Lines},
	write_symbol_to_db(?FILE_MARK, SrcFile, 0, State),
	{ok, ParseTree} = epp_dodger:parse_file(SrcFile),
	%io:format("~p",[ParseTree]),
	NewState = traverse_tree([ParseTree],State),
	% write the left over bytes
	Line = lists:nth(NewState#state.line_no, NewState#state.data),
	io:format(Db, "~s~n~n", [string:sub_string(Line, NewState#state.pos)]).


traverse_tree(TreeList, S=#state{}) ->
	lists:foldl( 
	fun(NodeList,S2) ->
		lists:foldl(fun(Node,State) ->
			%io:format("Node ~p~n", [Node]),
			{NewNode, NewState} = case erl_syntax:type(Node) of
				  atom -> process_atom(Node, State);
				  function -> process_function(Node,State);
				  application -> process_application(Node, State);
				  attribute -> process_attribute(Node,State);
				  _ -> {erl_syntax:subtrees(Node), State}
			  end,
			  traverse_tree(NewNode,NewState)
    	end, S2, NodeList)
	end, S, TreeList).


process_attribute(Node,S=#state{}) ->
	NewState = case erl_syntax_lib:analyze_attribute(Node) of 
		{ module, ModAtom } ->
			S#state{modname=atom_to_list(ModAtom)};
		_ -> S
    end,
 	{[],NewState}.


process_atom(Node, S=#state{}) ->
	AtomName = erl_syntax:atom_name(Node),
	%io:format("atom ~p len ~b~n",[AtomName,length(AtomName)]),
	LineNo = erl_syntax:get_pos(Node),
	if  LineNo > 0 ->
		NewState = write_symbol_to_db(?SYMBOL_MARK, AtomName, Node, S),
		%io:format("old state ~p, new state ~p~n",[debug_print_state(S), debug_print_state(NewState)]),
		{[], NewState};
	true ->
		{[],S}
	end.


process_function(Node, S=#state{}) ->
	try erl_syntax_lib:analyze_function(Node) of 
		{FAtom, FArity} -> 
			Fname = atom_to_list(FAtom),
		 	%io:format("function ~s~n",[Fname]),
			NewState1 = write_symbol_to_db(?FUNCTION_DEF_MARK, atom_to_list(FAtom), Node, S),
			[_FuncTree, ClauseTree ]=  erl_syntax:subtrees(Node),
			NewState2 = traverse_tree([ClauseTree], NewState1),
			NewState3 = write_symbol_to_db(?FUNCTION_END_MARK, "", Node, NewState2),
	        {[], NewState3}
	catch 
		syntax_error ->
			{[], S}
	end.
		
process_application(Node, S=#state{}) ->
	try erl_syntax_lib:analyze_application(Node) of 
		{ ModName, {Fname, Airity} } ->
			io:format("application mod ~p func ~p airity~p~n",[ModName,Fname,Airity]),
			Name = atom_to_list(Fname),
			NewState = write_symbol_to_db(?FUNCTION_CALL_MARK, Name, Node, S),
			[[_ApplicationOperator], ApplicationArgs ] = erl_syntax:subtrees(Node),
			{[ApplicationArgs],NewState};
		{ Fname, Arity } -> 
			io:format("application func ~p airity ~p ~n",[Fname,Arity]),
			Name = atom_to_list(Fname),
			NewState = write_symbol_to_db(?FUNCTION_CALL_MARK, Name, Node, S),
			[[_ApplicationOperator], ApplicationArgs ] = erl_syntax:subtrees(Node),
			{[ApplicationArgs],NewState}
	catch 
		syntax_error -> 
		   {[],S}
	end.

%% ====================================================================
%% Functions for writing to the file
%% ====================================================================

init_symbol_db(Db, TrailerOffset ) ->
	{ok, Cwd} = file:get_cwd(),
	Header = lists:flatten(io_lib:format("cscope ~s ~s -c ~10..0b~n", 
									[?CSCOPE_VERSION, Cwd,TrailerOffset])),
	io:format(Db,"~s",[Header]).


%% Format For File      
%% <file mark><file path>
%% <empty line>

write_symbol_to_db(?FILE_MARK, Fname, _Node , S=#state{}) ->
	io:format(S#state.db,"~s~s~n~n", [?FILE_MARK, Fname]);

% XXX: This assumes that there won't be two functions in the same line ... 
write_symbol_to_db(?FUNCTION_END_MARK, _Name, _Node, S=#state{}) ->
	% write the left over bytes
	Line = lists:nth(S#state.line_no, S#state.data),
	io:format(S#state.db, "~s~n~n", [string:sub_string(Line, S#state.pos)]),
	io:format(S#state.db,"~s~n~n", [?FUNCTION_END_MARK]),
	NewLine = S#state.line_no+1,
	S#state{line_no=NewLine,pos=1};

%% Format for other symbols
%% <line number><blank><non-symbol text>
%% <optional mark><symbol>
%% <non-symbol text>
%% repeat above 2 lines as necessary
%% <empty line>
%% Leading and trailing white space in  the  source  line  is
%% removed. Tabs are changed to blanks, and multiple blanks
%% are squeezed to a single  blank,  even  in  character  and
%% string  constants.
%% whitespaces are already removed when saving the lines in 
%% the state

write_symbol_to_db(Type, Name, Node, S=#state{}) when length(Name) > 0 ->
	Fd = S#state.db,
	LineNo = erl_syntax:get_pos(Node),
	Line = lists:nth(LineNo, S#state.data),
	SearchPos = case S#state.line_no == LineNo of 
		true -> S#state.pos;
		false -> 1 % new line, start from pos 1
    end,
	FoundLen = string:str( string:sub_string(Line, SearchPos), Name),
	case FoundLen > 0 of 
	   true ->
		 StartPos = 
		 if (LineNo =/= S#state.line_no) ->
	 			% we have moved to new line, complete the old line
		     	% and write the new line number
				OldLine = if S#state.line_no > 0 ->
					lists:nth(S#state.line_no, S#state.data);
					true -> ""
				end,
				io:format(Fd,"~s~n~n~b ", [string:substr(OldLine,S#state.pos), LineNo]),
				1;
			true ->  % same line , no update needed
	  			S#state.pos 
		 end, % (LineNo =/= S#state.line_no)
		 
		 
		 EndPos = StartPos + FoundLen -1,
		 NonSymbolData =  string:substr(Line, StartPos , EndPos - StartPos),
		 % write non symbol data
		 io:format(Fd,"~s~n",[NonSymbolData]),
		 % write symbol data
		 io:format(Fd,"~s~s~n",[Type,Name]),
		 S#state{line_no=LineNo, pos=EndPos + length(Name)};
	  false -> % cannot find this name
		 S
	end; % Foundlen > 0
		
	
write_symbol_to_db(_Type, _Name, _Node, S=#state{}) ->
	S.

write_symbol_trailer_to_db(Db,Files) ->
   	io:format(Db, "~s~n", [?SYMBOL_END]),
	TrailerOffset = filelib:file_size(?OUTPUT_FILE),
	io:format(Db, "1~n.~n0~n~p~n~p~n", [length(Files), lists:flatlength(Files) + length(Files)]),
	lists:foreach(fun(Fname) -> io:format(Db,"~s~n",[Fname]) end, Files),
	file:close(Db),
	{ok , Db2} = file:open(?OUTPUT_FILE, [read,write]),
	%io:format("Trailer offset ~b",[TrailerOffset]),
	init_symbol_db(Db2, TrailerOffset),
	file:close(Db2).


%% ====================================================================
%% Other utility functions
%% ====================================================================

% splits string into a list of strings delimeted by newline.
% blank lines are saved as empty lists. The library function
% string:tokens() discards blank lines. 

split_data_to_lines(Data) ->
	split_data_to_lines(Data,[],[]).

split_data_to_lines([],Acc,Out)->
	lists:reverse([Acc|Out]);

split_data_to_lines([Ch|Rem], Acc, Out) ->
	case Ch == $\n of
		true -> split_data_to_lines(Rem, [], [ lists:reverse(remove_spaces(Acc)) | Out ]);
		false -> split_data_to_lines(Rem, [Ch|Acc], Out )
	end.

remove_spaces(String) ->
	re:replace(String, "\\s+", " ", [global, {return, list}]).

debug_print_state(S=#state{}) ->
	{S#state.line_no, S#state.pos}.