all: compile

compile:
	sbt assembly

test:
	sbt test

clean:
	sbt clean
	rm $(wildcard *.jar) $(wildcard *.s)

.PHONY: all compile clean
