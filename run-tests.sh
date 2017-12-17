#!/bin/bash
set -ex

tests/test-gzip.scm
tests/test-huffman.scm

echo All tests passed
