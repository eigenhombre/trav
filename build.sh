#!/bin/bash

set -e

#clojure -Aoutdated -m depot.outdated.main
clojure -Atest
sleep 5
clojure -Aoutdated
