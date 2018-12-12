#!/bin/bash

clojure -A:aot
clojure -A:pack mach.pack.alpha.aws-lambda -C:aot trav.zip
