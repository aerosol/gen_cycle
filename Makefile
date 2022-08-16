.PHONY: all deps compile test

all: test

deps:
	@./rebar get-deps

compile: deps
	@./rebar compile

test: compile
	@./rebar ct
