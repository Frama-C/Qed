#!/bin/sh

set -e

BRANCH=$1

if [ "$BRANCH" == "" ];
then
    BRANCH="master"
fi

echo "Branching"

git checkout -B frama-c/$BRANCH

echo "Archiving 'frama-c:$BRANCH'"

git archive \
    --remote git@git.frama-c.com:frama-c/frama-c.git \
    --format=tgz -o .frama-c.tgz $BRANCH

echo "Extracting the archive"

rm -fr .frama-c.tmp
mkdir .frama-c.tmp
cd .frama-c.tmp && tar zxf ../.frama-c.tgz && cd ..

echo "Importing OCaml files"

rm -fr src
mkdir -p src
rm -f .frama-c.tmp/src/plugins/qed/QedGui.ml
cp .frama-c.tmp/src/plugins/qed/*.ml* src/

echo "Cleaning"

rm -fr .frama-c.tmp .frama-c.tgz
make lint

echo "Done."
