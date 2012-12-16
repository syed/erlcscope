-define(INPUT_FILE, "cscope.files").
-define(OUTPUT_FILE, "cscope.out").


%% Markers used in cscope database
-define(FILE_MARK, "\t@").
-define(DEFINE_MARK, "#").
-define(DEFINE_END_MARK, ")").
-define(FUNCTION_CALL_MARK, "`").
-define(FUNCTION_DEF_MARK, "$").
-define(FUNCTION_END_MARK, "}").
-define(INCLUDE_MARK, "~")
-define(RECORD_DEF_MARK, "m").


%cscope specific constants
-define(CSCOPE_VERSION, "15").
-define(SYMBOL_END, ?FILE_MARK).
