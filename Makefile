clean:
	rm *~ *.beam *.access *.auth *.log *.dump|| true
all:
	erlc *.erl
