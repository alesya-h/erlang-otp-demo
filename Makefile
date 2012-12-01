.PHONY: build clean init run tags

all: build

build: deal_server.beam deal_server_sup.beam deal_server_test.beam deal_system.beam deal_system_sup.beam deals_yaws.beam deals_yaws_sup.beam time_helper.beam web_helper.beam

clean:
	rm *~ *.beam *.access *.auth *.log *.dump TAGS > /dev/null 2>&1|| true

init:
	cat test.in|awk '{printf "curl -d name=%s -d date=%s -d time=%s -d cost=%s -d quantity=%s localhost:8080/deals.yaws\n", "ECHO", "2012-12-01", $$1, $$2, $$3}'|sh

run: build
	erl -s deal_system

tags:
	ctags -e *.hrl *.erl

%.beam: %.erl
	erlc $+
