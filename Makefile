all:
	parrot choco.pir

pasm:
	parrot -o - choco.pir
