#!/usr/bin/env bash

set -e

PACKAGES=${HOME}/.emacs.d/config/packages.el
ELGET=${HOME}/.emacs.d/el-get

if [ ! -d ${ELGET} ]; then
    mkdir ${ELGET}
    git clone https://github.com/dimitri/el-get.git ${ELGET}/el-get
fi

emacs --batch -l ${PACKAGES} -f dotfiles-sync
rm ${ELGET}/.loaddefs.*
emacs --batch -l ${PACKAGES}
