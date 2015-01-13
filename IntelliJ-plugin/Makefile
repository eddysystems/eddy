GRAM = tarski/src/tarski/eddy.gram
PARSE = tarski/src/tarski/ParseEddy.java
ACTIONS = tarski/src/tarski/ParseEddyActions.scala
AMBIGUITY = out/artifacts/ambiguity_jar/ambiguity.jar

all: $(PARSE) $(ACTIONS)
jar: eddy.jar

$(PARSE): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) $(GRAM) > $(PARSE)

$(ACTIONS): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) -a $(GRAM) > $(ACTIONS)

eddy.jar: eddy.zip shrink.pro
	proguard @shrink.pro

.PHONY: install
install:
	cp eddy.jar "${HOME}/Library/Application Support/IdeaIC13"

.PHONY: clean
clean:
	rm -f $(PARSE) $(ACTIONS)
