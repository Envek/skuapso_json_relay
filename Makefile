ifeq ($(PREFIX),)
	PWD := "./skuapso"
else
	PWD := "$(PREFIX)/skuapso"
endif
ifeq ($(SKUAPSO_GIT_PREFIX),)
	GIT_PREFIX := "https://github.com/skuapso/"
else
	GIT_PREFIX := $(SKUAPSO_GIT_PREFIX)
endif
APPSFILES := $(wildcard ebin/*.app lib/*/ebin/*.app)

.PHONY: compile install clean deps rebar_% config

compile:
	@rebar compile

deps:
	@SKUAPSO_GIT_PREFIX=$(GIT_PREFIX) rebar get-deps
	@SKUAPSO_GIT_PREFIX=$(GIT_PREFIX) rebar update-deps

clean:
	@rebar clean

rebar_%:
	@rebar $*
