BASE=$(abspath .)
REBAR ?= ./rebar

APP_DIR=$(BASE)/apps

.PHONY: all compile clean cleanall test dep update

compile:
	$(REBAR) compile

compile-app-only:
	$(REBAR) compile skip_deps=true

all: dep compile

dep:
	$(REBAR) get-deps

update:
	$(REBAR) update

clean: clean-test
	$(REBAR) clean

cleanall: clean-dep clean-files clean

test: eunit ct

xref:
	@echo "=================== Xref =============================="
	$(REBAR) xref skip_apps=build skip_deps=true

eunit:
	$(REBAR) eunit skip_apps=build skip_deps=true

ct:
	@echo "=================== Common Test ======================="
	$(REBAR) ct skip_apps=build skip_deps=true


clean-dep:
	$(REBAR) delete-deps

clean-test:
	$(REBAR) clean skip_apps=build skip_deps=true

clean-files:
	find . -name '*~' -exec rm -f -- \{\} \;
	find . -name 'erl_crash.dump' -exec rm -f -- \{\} \;
	find . -name 'MnesiaCore.*' -type f -exec rm -rf -- \{\} \;
	find $(APP_DIR) -depth -type d -name 'logs' -exec rm -rf -- \{\} \;
