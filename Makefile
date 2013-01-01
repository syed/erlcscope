CP=cp
CHMOD=chmod
DEST_BIN=/usr/local/bin
PROG=erlcscope
REBAR=./rebar

erlcscope:  src/
	$(REBAR) compile
	$(REBAR) escriptize
	$(CHMOD) 755 $@

install: erlcscope
	$(CP) $(PROG) $(DEST_BIN)

clean:
	$(RM) $(PROG)
	$(REBAR) clean
