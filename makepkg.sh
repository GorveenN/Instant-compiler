#!/bin/bash
rm -rf pk394334
mkdir pk394334
cp -r stdlib src package.yaml stack.yaml Makefile README.md lattests pk394334
rm -rf pk394334/lattests/others
cd pk394334
tar czvf ../pk394334.tar.gz *
cd ..
rm -rf pk394334
