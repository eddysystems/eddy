GRAM = tarski/src/tarski/eddy.gram
PARSE = tarski/src/tarski/ParseEddy.java
ACTIONS = tarski/src/tarski/ParseEddyActions.scala
AMBIGUITY = out/artifacts/ambiguity_jar/ambiguity.jar

MODIFIED = `git status --short | grep -c ""` 
COMMIT = `git log --no-color --oneline --no-abbrev | head -n1 | awk '{ print $$1; }'`

all: $(PARSE) $(ACTIONS)
jar: eddy.jar

$(PARSE): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) $(GRAM) > $@

$(ACTIONS): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) -a $(GRAM) > $@

.PHONY: commit
commit: 
	@echo $(COMMIT) 

eddy.jar: eddy.zip .idea/shrink.pro
	proguard @shrink.pro

.PHONY: install
install:
	cp eddy.jar "${HOME}/Library/Application Support/IdeaIC13"

.PHONY: clean
clean:
	rm -f $(PARSE) $(ACTIONS)
