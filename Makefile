REBAR = ~/rebar3/rebar3
all: compile
compile: $(REBAR)
	$(REBAR) compile
test: $(REBAR)
	$(REBAR) eunit
$(REBAR):
	cd rebar3/ ; ./bootstrap
mrproper:
	rm -rf rebar.lock  _build
clean:
	rm -f rebar.lock `find _build -name *.beam`
dialyzer: $(REBAR)
	$(REBAR) dialyzer
