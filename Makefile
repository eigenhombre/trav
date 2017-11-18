all: tf

uberjar: target/trav.jar
	lein überjar

clean:
	cd tf && terraform destroy
	rm target/trav.jar

.PHONY: tf
tf:
	cd tf && terraform apply -auto-approve
