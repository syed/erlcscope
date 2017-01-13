%% Some special cases to check for bugs in erlcscope
%%
-module(special_record).

%% This exhibited a crash in erlscope:write_symbol_to_db/4. The crash:
%%
%% =ERROR REPORT==== 13-Jan-2017::23:26:39 ===
%% Error in process <0.1167.0> with exit value:
%% {badarg,[{array,get,2,[{file,"array.erl"},{line,650}]},
%%          {erlcscope,write_symbol_to_db,4,
%%                     [{file,"src/erlcscope.erl"},{line,268}]},
%%          {erlcscope,process_record,2,[{file,"src/erlcscope.erl"},{line,179}]},
%%          {erlcscope,'-traverse_tree/2-fun-0-',2,
%%                     [{file,"src/erlcscope.erl"},{line,95}]},
%%          {lists,foldl,3,[{file,"lists.erl"},{line,1263}]},
%%          {erlcscope,'-main/1-fun-1-',1,[{file,"src/erlcscope.erl"},{line,33}]},
%%          {erlcscope,pmap_lim_run,3,[{file,"src/erlcscope.erl"},{line,67}]}]}
-record('', {}).

%% This is allowed as well and should not lead to a crash.
''() ->
	 ok.
