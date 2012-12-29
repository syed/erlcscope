CP=cp
DEST_BIN=~/bin
PROG=erlcscope
REBAR=./rebar

erlcscope: 
	$(REBAR) compile
	$(REBAR) escriptize

install:
	$(CP) $(PROG) $(DEST_BIN)

clean:
	$(RM) $(PROG)
	$(REBAR) clean
