#!/bin/bash

set -e
echo Checking for outdated dependencies...
ancient=$(clojure -Aoutdated)
if [[ "$ancient" != *"All up to date"* ]]; then
    echo $ancient
    exit 1
fi
echo Running Kibit style checker...
kibit=$(clojure -Akibit)
if [[ "$kibit" != "" ]]; then
    echo $kibit
    exit 1
fi
echo "Running Eastwood style checker..."
clojure -Aeastwood
echo "Running unit tests..."
clojure -Atest
