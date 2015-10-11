GRAM = tarski/src/tarski/eddy.gram
PARSE = tarski/src/tarski/ParseEddy.java
ACTIONS = tarski/src/tarski/ParseEddyActions.scala
AMBIGUITY = out/artifacts/ambiguity_jar/ambiguity.jar

MODIFIED = `git status --short | grep -c ""` 
COMMIT = `git log --no-color --oneline --no-abbrev | head -n1 | awk '{ print $$1; }'`
GITMOD = $(strip $(shell git status --porcelain | wc -l | sed 's/\w*\(0\)[^0-9]*//'))
GITLOC = $(strip $(shell git status -b --porcelain | head -n 1 | awk '{ print $$3 }'))
DIRTY = $(if $(GITMOD),-dirty,)
LOCAL = $(if $(GITLOC),-local,)
VERSION = $(strip $(subst release-,,$(shell git describe --tags)))

PROGUARD = proguard

all: $(PARSE) $(ACTIONS)
jar: eddy.jar

.PHONY: version
version: 
	@echo $(VERSION)

$(PARSE): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) $(GRAM) > $@

$(ACTIONS): $(AMBIGUITY) $(GRAM)
	java -jar $(AMBIGUITY) -a $(GRAM) > $@

.PHONY: commit
commit: 
	@echo $(COMMIT)$(DIRTY)$(LOCAL)

eddy.jar: eddy.zip shrink.pro
	$(PROGUARD) @shrink.pro

.PHONY: install
install: eddy.jar
	find "${HOME}/Library/Application Support" -name "IdeaIC*" -print0 | xargs -0 -n 1 cp eddy.jar

.PHONY: uninstall
uninstall:
	find "${HOME}/Library/Application Support" -path "*/IdeaIC*/eddy.jar" -print0 | xargs -0 rm

.PHONY: clean
clean:
	rm -f $(PARSE) $(ACTIONS)
