all: tf target/trav.jar

uberjar:
	make target/trav.jar

target/trav.jar: src/trav/*.clj
	lein überjar

clean:
	cd tf && terraform destroy
	rm target/trav.jar

.PHONY: tf
tf: target/trav.jar
	cd tf && terraform apply -auto-approve
