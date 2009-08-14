#TARGET = choco.pir
#TARGET = continuation-interpreter.pir
#TARGET = fast-interpreter.pir
#TARGET = interpreter.pir
#TARGET = byte-code-interpreter.pir
TARGET = chocolisp.pir

all:
	parrot $(TARGET)

trace:
	parrot -t $(TARGET)

pasm:
	parrot -o - $(TARGET)

e:
	parrot -E $(TARGET)
