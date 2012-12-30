CP=cp
DEST_BIN=/usr/local/bin
PROG=erlcscope
REBAR=./rebar

erlcscope:  src/
	$(REBAR) compile
	$(REBAR) escriptize

install:
	$(CP) $(PROG) $(DEST_BIN)

clean:
	$(RM) $(PROG)
	$(REBAR) clean
