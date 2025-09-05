#!/bin/bash

echo "Building optimized binaries..."
echo "------------------------"

# Build Go binary
go build -ldflags="-s -w" -o solution-go solution.go
echo "Go binary built"

# Build Common Lisp binary
sbcl --load build-lisp.lisp --quit
echo "Common Lisp binary built"

echo ""
echo "Running Internal Benchmarks"
echo "==========================="
ruby solution.rb
python3 solution.py
./solution-go
./solution-lisp

echo ""
echo "Running External Benchmarks (with startup time)"
echo "=============================================="

echo -n "Ruby total time: "
time -p ruby solution.rb 2>&1 | grep real | awk '{print $2*1000 "ms"}'

echo -n "Python total time: "
time -p python3 solution.py 2>&1 | grep real | awk '{print $2*1000 "ms"}'

echo -n "Go total time: "
time -p ./solution-go 2>&1 | grep real | awk '{print $2*1000 "ms"}'

echo -n "Common Lisp total time: "
time -p ./solution-lisp 2>&1 | grep real | awk '{print $2*1000 "ms"}'
