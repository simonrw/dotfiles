#!/bin/bash


# first get the most up to date version of this repo
git pull

# now update the submodules
git submodule init
git submodule update


echo "Calling init..."
git submodule foreach git submodule init
echo
echo "Calling update..."
git submodule foreach git submodule update
