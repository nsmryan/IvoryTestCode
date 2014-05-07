#! /bin/bash
set -e
cabal build
./dist/build/IvoryTest/IvoryTest
make #upload
