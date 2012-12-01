.PHONY: build clean run

all: build

build: deal_server.beam deal_server_sup.beam deal_server_test.beam deal_system.beam deal_system_sup.beam deals_yaws.beam deals_yaws_sup.beam time_helper.beam web_helper.beam

run: build
	erl -s deal_system

clean:
	rm *~ *.beam *.access *.auth *.log *.dump || true

deal_server.beam:       deal_server.erl
	erlc deal_server.erl
deal_server_sup.beam:   deal_server_sup.erl
	erlc deal_server_sup.erl
deal_server_test.beam:  deal_server_test.erl
	erlc deal_server_test.erl
deal_system.beam:       deal_system.erl
	erlc deal_system.erl
deal_system_sup.beam:   deal_system_sup.erl
	erlc deal_system_sup.erl
deals_yaws.beam:        deals_yaws.erl
	erlc deals_yaws.erl
deals_yaws_sup.beam:    deals_yaws_sup.erl
	erlc deals_yaws_sup.erl
time_helper.beam:       time_helper.erl
	erlc time_helper.erl
web_helper.beam:        web_helper.erl
	erlc web_helper.erl
