#!/bin/bash

set -e
ancient=$(clojure -Aoutdated)
if [[ "$ancient" != *"All up to date"* ]]; then
    echo $ancient
    exit 1
fi
clojure -Atest
