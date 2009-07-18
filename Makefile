all:
	parrot choco.pir

trace:
	parrot -t choco.pir

pasm:
	parrot -o - choco.pir
