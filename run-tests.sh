#!/bin/bash
set -ex

tests/test-gzip.scm
tests/test-huffman.scm
tests/test-zlib.scm

echo All tests passed
