#!/usr/bin/env bash

set -e

GITPROJECTDIR=${HOME}/git

create_project_dir() {
    if [ -d "${1}" ]; then
	echo "Project directory exists" >&2
	exit 1
    else
	mkdir -p "${1}"
    fi
}

initialise_git() {
    git init --bare --quiet
}

initialise_local_repo() {
    test -d .git || git init
}

add_remote() {
    git remote add local "${1}" 2>/dev/null || true
    echo "Add remote 'local'"
}

main() {
    currentdirname=$(basename $(pwd))
    projectname="${GITPROJECTDIR}/${currentdirname}.git"
    
    echo "Creating project: ${projectname}"
    create_project_dir "${projectname}"
    (cd ${projectname}
     initialise_git
    )

    initialise_local_repo
    add_remote "${projectname}"
}

main "$@"
