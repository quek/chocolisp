#TARGET = choco.pir
#TARGET = continuation-interpreter.pir
#TARGET = fast-interpreter.pir
TARGET = interpreter.pir

all:
	parrot $(TARGET)

trace:
	parrot -t $(TARGET)

pasm:
	parrot -o - $(TARGET)
