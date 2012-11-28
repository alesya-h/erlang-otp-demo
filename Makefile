clean:
	rm *~ *.beam
yaws:
	yaws --conf yaws.conf -i --heart -sname deals_yaws
