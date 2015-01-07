GRAM = tarski/src/tarski/eddy.gram
PARSE = tarski/src/tarski/ParseEddy.java
ACTIONS = tarski/src/tarski/ParseEddyActions.scala
AMBIGUITY = out/artifacts/ambiguity_jar/ambiguity.jar

all: $(PARSE) $(ACTIONS)

$(PARSE): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) $(GRAM) > $(PARSE)

$(ACTIONS): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) -a $(GRAM) > $(ACTIONS)

.PHONY: clean
clean:
	rm -f $(PARSE) $(ACTIONS)
