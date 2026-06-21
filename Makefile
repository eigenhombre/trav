.PHONY: all
all: uberjar

.PHONY: uberjar
uberjar:
	make target/trav.jar

target/trav.jar: src/trav/*.clj
	lein überjar

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

.PHONY: clean
clean:
	rm target/trav.jar
