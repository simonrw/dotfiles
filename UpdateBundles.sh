#!/bin/bash

cd ${HOME}/.vim
git submodule update

echo "Calling init..."
git submodule foreach git submodule init
echo
echo "Calling update..."
git submodule foreach git submodule update
