#!/bin/bash

set -e

#clojure -Aoutdated -m depot.outdated.main
clojure -Atest -m cognitect.test-runner
