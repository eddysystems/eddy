DIR = tarski/src/ambiguity
AMBIGUITY = out/artifacts/tarski_jar/tarski.jar

all: $(DIR)/ParseEddy.scala

$(DIR)/ParseEddy.scala: $(AMBIGUITY) $(DIR)/eddy.gram
	java -jar $(AMBIGUITY) $(DIR)/eddy.gram > $(DIR)/ParseEddy.scala

.PHONY: clean
clean:
	rm -f $(DIR)/ParseEddy.scala
