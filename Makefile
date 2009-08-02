#TARGET = choco.pir
#TARGET = continuation-interpreter.pir
TARGET = fast-interpreter.pir

all:
	parrot $(TARGET)

trace:
	parrot -t $(TARGET)

pasm:
	parrot -o - $(TARGET)
