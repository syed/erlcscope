%% @author Syed Ahmed
%% @doc Program to parse erlang files and build a cscope database.
-module(erlcscope).
-include("erlcscope.hrl").
-include_lib("kernel/include/file.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([start_link/0,main/1, pmap_lim_run/3]).

start_link()->
	Pid = spawn_link(fun main/1),
	{ok,Pid}.

main(InPath)->
	case filelib:is_regular(filename:join(InPath, ?INPUT_FILE)) of
	 	true->	
			io:format("Using source files from ~s~n",[?INPUT_FILE]),
			{ok, Data} = file:read_file(?INPUT_FILE),
	 		Files = lists:sort(string:tokens(binary_to_list(Data), ?NEWLINE_SEP));
		false ->
			io:format("Searching for source files ...~n"),
			Files = find_source_files(InPath),
			{ok, FilesFd} = file:open(?INPUT_FILE, [write]),
			lists:foreach(fun(File) -> io:format(FilesFd, "~s~n", [File]) end, Files),
			file:close(FilesFd)
	end,
	{ok, Db} = file:open(?OUTPUT_FILE,[write]),
	init_symbol_db(Db,0),
	Sched = erlang:system_info(schedulers),
	Entries = pmap_lim(fun(Fname) ->
			S = build_symbol_db_from_file(Fname),
			list_to_binary(lists:reverse(S#state.entries))
		end, 
	Files, Sched),
	lists:foreach(fun(E) -> file:write(Db, E) end, Entries),
	write_symbol_trailer_to_db(Db, Files),
	io:format("Processed ~b files~n", [length(Files)])
	.

pmap_lim(F, L, Lim) ->
	Self = self(),
	NumberedL = lists:zip(lists:seq(0, length(L)-1), L),
	{Running, Waiting} = lists:split(Lim, NumberedL),
	[spawn(?MODULE, pmap_lim_run, [F, Self, I]) || I <- Running],
	pmap_lim1(F, Waiting, array:new(length(L)), Lim)
	.

pmap_lim1(_F, [], Results, 0) ->
	array:to_list(Results);
pmap_lim1(F, [], Results, Outstanding) ->
	receive
		{N, R} ->
			pmap_lim1(F, [], array:set(N, R, Results), Outstanding - 1)
	end
	;
pmap_lim1(F, [Next | Waiting], Results, Lim) ->
	receive
		{N, R} ->
			spawn(?MODULE, pmap_lim_run, [F, self(), Next]),
			pmap_lim1(F, Waiting, array:set(N, R, Results), Lim)
	end
	.

pmap_lim_run(F, ReplyTo, {N, I}) ->
	R = F(I),
	ReplyTo ! {N, R}
	.

%% ====================================================================
%% Parsing and processing functions
%% ====================================================================

build_symbol_db_from_file(SrcFile) ->
	{ok, FileData} = file:read_file(SrcFile),
	%io:format("processing ~p~n",[SrcFile]),
	FileData1 = re:replace(FileData, "[\r \t]+", " ", [global, {return, binary}]),
	Lines = binary:split(FileData1, <<"\n">>, [global]),
	State = #state{data=array:from_list(Lines)},
	NewState = write_symbol_to_db(?FILE_MARK, SrcFile, 0, State),
	{ok, ParseTree} = epp_dodger:parse_file(SrcFile),
	%io:format("~p",[ParseTree]),
	traverse_tree([ParseTree],NewState).


traverse_tree(TreeList, S=#state{}) ->
	lists:foldl( 
	fun(NodeList,S2) ->
		lists:foldl(fun(Node,State) ->
			%io:format("Node ~p~n", [Node]),
			{NewNode, NewState} = case erl_syntax:type(Node) of
				  application -> process_application(Node, State);
				  atom -> process_atom(Node, State);
				  attribute -> process_attribute(Node,State);
				  function -> process_function(Node,State);
				  variable -> process_variable(Node,State);
				  _ -> {erl_syntax:subtrees(Node), State}
			  end,
			  traverse_tree(NewNode,NewState)
    	end, S2, NodeList)
	end, S, TreeList).

process_application(Node, S=#state{}) ->
	%io:format("application ~p~n",[erl_syntax_lib:analyze_application(Node)]),
	try erl_syntax_lib:analyze_application(Node) of 
		{ _ModName, {Fname, _Airity} } ->
			Name = atom_to_list(Fname),
			NewState = write_symbol_to_db(?FUNCTION_CALL_MARK, Name, Node, S),
			[[_ApplicationOperator], ApplicationArgs ] = erl_syntax:subtrees(Node),
			{[ApplicationArgs],NewState};
		{ Fname, _Arity } -> 
			Name = atom_to_list(Fname),
			NewState = write_symbol_to_db(?FUNCTION_CALL_MARK, Name, Node, S),
			[[_ApplicationOperator], ApplicationArgs ] = erl_syntax:subtrees(Node),
			{[ApplicationArgs],NewState};
		_ ->
			{[],S}
	catch 
		syntax_error -> 
		   {[],S}
	end.

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


process_attribute(Node,S=#state{}) ->
	%io:format("node ~p~n",[Node]),
	%io:format("attr name ~p~n",[erl_syntax:atom_value(erl_syntax:attribute_name(Node))]),
	Type = erl_syntax:atom_value(erl_syntax:attribute_name(Node)),
	case Type of
		define ->
			process_define(Node,S);
		record ->
			process_record(Node,S);
		_ -> 
			{[],S}
	end.
 	
process_define(Node,S=#state{}) ->
	%io:format("subtree ~p~n",[erl_syntax:attribute_arguments(Node)]),
	[Def | Subtree] = erl_syntax:attribute_arguments(Node),
	Name = get_define_name(Def),
	%io:format("name ~p len ~p pos ~p~n",[Name,length(Name), erl_syntax:get_pos(Def)]),
	NewState1 = write_symbol_to_db(?MACRO_MARK, Name, Def, S),
	NewState2 = traverse_tree([Subtree], NewState1),
	NewState3 = write_symbol_to_db(?MACRO_END_MARK, "" , Node, NewState2),
	{[],NewState3}.

process_function(Node, S=#state{}) ->
	try erl_syntax_lib:analyze_function(Node) of 
		{FAtom, _FArity} -> 
			Fname = atom_to_list(FAtom),
		 	%io:format("function ~s~n",[Fname]),
			NewState1 = write_symbol_to_db(?FUNCTION_DEF_MARK, Fname, Node, S),
			[_FuncTree, ClauseTree ]=  erl_syntax:subtrees(Node),
			NewState2 = traverse_tree([ClauseTree], NewState1),
			NewState3 = write_symbol_to_db(?FUNCTION_END_MARK, "", Node, NewState2),
	        {[], NewState3}
	catch 
		syntax_error ->
			{[], S}
	end.
		

process_record(Node, S=#state{}) ->
	try erl_syntax_lib:analyze_record_attribute(Node) of
		{RecName, _Fields}  -> 
			NewState1 = write_symbol_to_db(?RECORD_DEF_MARK, atom_to_list(RecName), Node, S),
			{[] , write_symbol_to_db(?RECORD_DEF_END_MARK, "", Node, NewState1) }
	catch 
		syntax_error ->
			{[],S}
	end.

process_variable(Node, S=#state{}) ->
	VarName = erl_syntax:variable_literal(Node),
	NewState = write_symbol_to_db(?SYMBOL_MARK, VarName, Node, S),
	{[],NewState}.
  
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

write_symbol_to_db(?FILE_MARK, Fname, _Node , S=#state{entries = Entries}) ->
	Line = f("~s~s~n~n", [?FILE_MARK, Fname]),
	S#state{entries = [Line | Entries]};

%% ============================================================
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
%% ============================================================

write_symbol_to_db(Type, Name, Node, S=#state{entries = Entries}) when length(Name) > 0 ->
	LineNo = erl_syntax:get_pos(Node),
	%io:format("LineNo ~p~n",[LineNo]),
	Line = binary_to_list(array:get(LineNo-1, S#state.data)),
	%io:format("Line ~p~n",[Line]),
	SearchPos = case S#state.line_no == LineNo of 
		true -> S#state.pos;
		false -> 1 % new line, start from pos 1
	end,
	FoundLen = string:str( string:sub_string(Line, SearchPos), Name),
	%io:format("fount ~p at ~p~n",[Name,FoundLen]),
	case FoundLen > 0 of 
	   true ->
		{StartPos, L1} = 
		 if (LineNo =/= S#state.line_no) ->
	 			% we have moved to new line, complete the old line
		     	% and write the new line number
				OldLine = if S#state.line_no > 0 ->
					binary_to_list(array:get(S#state.line_no-1, S#state.data));
					true -> ""
				end,
				{1, f("~s~n~n~b ", [string:substr(OldLine,S#state.pos), LineNo])};
			true ->  % same line , no update needed
				{S#state.pos, ""}
		 end, % (LineNo =/= S#state.line_no)
		 
		 
		 EndPos = StartPos + FoundLen -1,
		 NonSymbolData =  string:substr(Line, StartPos , EndPos - StartPos),
		 % write non symbol data
		 L2 = f("~s~n",[NonSymbolData]),
		 % write symbol data
		 L3 = f("~s~s~n",[Type,Name]),
		 S#state{line_no=LineNo, pos=EndPos + length(Name), entries = [lists:flatten([L1, L2, L3]) | Entries]};
	  false -> % cannot find this name
		 S
	end; % Foundlen > 0

% XXX: This assumes that there won't be two functions in the same line ...
% called for CLOSE of FUNCTION/MACRO
 
write_symbol_to_db(Type, "", _Node, S=#state{entries = Entries}) ->
	% write the left over bytes
	Line = binary_to_list(array:get(S#state.line_no-1, S#state.data)),
	L = f("~s~n~s~n~n", [string:sub_string(Line, S#state.pos), Type]),
	%NewLine = S#state.line_no+1,
	%S#state{line_no=NewLine,pos=1};
	S#state{entries = [L | Entries]};
	
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

% returns the name in -define(Name,...)

get_define_name(Def) ->
	case erl_syntax:type(Def) of
		variable ->
			erl_syntax:variable_literal(Def);
		application ->
			get_define_name(erl_syntax:application_operator(Def));
		atom -> 
			erl_syntax:atom_literal(Def)
	end.

% recursively find erlang files, returns a list of filenames

find_source_files(Path) ->
	{ok, Files} = file:list_dir(Path),
	lists:foldr(fun(F,Acc) ->
				File = filename:join(Path, F),
				{ok, Info=#file_info{}} = file:read_file_info(File),
				case Info#file_info.type of 
					directory -> 
						find_source_files(File) ++ Acc;
					regular ->
						case lists:member(filename:extension(File), ?ERL_EXTENSIONS) of 
							true -> [File|Acc];
							false -> Acc
						end;
					_ -> 
						Acc
				end % case end
				end, [], Files).


debug_print_state(S=#state{}) ->
	io:format("line ~p, pos ~p~n",[S#state.line_no, S#state.pos]).

f(F, A) ->
	lists:flatten(io_lib:format(F, A))
	.
