#!/bin/bash

clojure -A:pack mach.pack.alpha.capsule \
        trav.jar \
        -e build-dir \
        --application-id eigenhombre.trav \
        --application-version "1" \
        -m trav.core
