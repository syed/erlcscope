-record(state, { pos=1, line_no=0, data=[], db}).


%% Markers used in cscope database
-define(FILE_MARK, "\t@").
-define(MACRO_MARK, "\t#").
-define(MACRO_END_MARK, "\t)").
-define(FUNCTION_CALL_MARK, "\t`").
-define(FUNCTION_DEF_MARK, "\t$").
-define(FUNCTION_END_MARK, "\t}").
-define(INCLUDE_MARK, "\t~").
-define(RECORD_DEF_MARK, "\tm").
-define(SYMBOL_MARK, "").


%% cscope specific constants
-define(CSCOPE_VERSION, "15").
-define(SYMBOL_END, ?FILE_MARK).
-define(NEWLINE_SEP, "\n").

%% other constants
-define(INPUT_FILE, "cscope.files").
-define(OUTPUT_FILE, "cscope.out").
-define(ERL_EXTENSIONS, [".erl",".hrl"]).
