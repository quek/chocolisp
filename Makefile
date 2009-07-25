all:
	parrot continuation-interpreter.pir
#	parrot choco.pir

trace:
	parrot -t choco.pir

pasm:
	parrot -o - choco.pir
