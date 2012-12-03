.PHONY: build clean init run tags

all: build tags

build: deal_server.beam deal_server_sup.beam deal_server_test.beam deal_system.beam deal_system_sup.beam deals_yaws.beam deals_yaws_sup.beam time_helper.beam web_helper.beam

clean:
	rm *~ *.beam *.access *.auth *.log *.dump TAGS > /dev/null 2>&1|| true

init:
	cat test.in|awk '{printf "curl -d name=%s -d date=%s -d time=%s -d cost=%s -d quantity=%s localhost:8080/deals.yaws\n", "ECHO", "2012-04-11", $$1, $$2, $$3}'|sh &>/dev/null

run: build
	erl -sname deal_system -s deal_system -noinput

tags:
	ctags -e *.hrl *.erl

%.beam: %.erl
	erlc $+
