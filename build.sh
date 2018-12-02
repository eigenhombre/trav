#!/bin/bash

set -e

#clojure -Aoutdated -m depot.outdated.main
clj -Atest
