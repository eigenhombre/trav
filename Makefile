SOURCES := $(shell find src -name '*.clj')

.PHONY: all
all: uber

.PHONY: uber
uber: trav.jar

trav.jar: $(SOURCES) deps.edn build.clj
	clojure -T:build uber

.PHONY: deps
deps:
	clojure -P

.PHONY: ancient
ancient:
	clojure -M:outdated

.PHONY: lint
lint:
	clojure -M:kibit
	clojure -M:eastwood

.PHONY: format
format:
	cljfmt fix src

.PHONY: test
test:
	clojure -M:test

.PHONY: install
install: trav.jar
	@if [ ! -d "$${BINDIR:-$$HOME/bin}" ]; then \
		echo "Error: Install directory $${BINDIR:-$$HOME/bin} does not exist."; \
		echo "Please create it or set BINDIR to an existing directory."; \
		exit 1; \
	fi
	cp trav trav.jar $${BINDIR:-$$HOME/bin}/

.PHONY: run
run:
	clojure -M:run

.PHONY: clean
clean:
	rm -rf target/ trav.jar .cpcache/
