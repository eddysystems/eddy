DIR = tarski/src/ambiguity
GRAM = tarski/src/ambiguity/eddy.gram
PARSE = tarski/src/ParseEddy.scala
AMBIGUITY = out/artifacts/tarski_jar/tarski.jar

all: $(PARSE)

$(PARSE): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) $(GRAM) > $(PARSE)

.PHONY: clean
clean:
	rm -f $(PARSE)
