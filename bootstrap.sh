#!/usr/bin/env bash

set -e

command_exists() {
    command -v $1 >/dev/null
}

ansible_exists() {
    command_exists ansible-playbook
}

homebrew_exists() {
    command_exists brew
}

bootstrap_ansible() {
    brew install ansible
}

bootstrap_homebrew() {
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
}

provision() {
    case $OSTYPE in
        linux*)
            ansible-playbook -i hosts provisioning/site.yml
            ;;
        darwin*)
            echo "Root password may be needed to alter system plists"
            ansible-playbook -i hosts provisioning/site.yml --ask-become-pass "$@"
            ;;
    esac
}

install_ansible_if_required() {
    `ansible_exists` && echo "Ansible found" || bootstrap_ansible
}

install_homebrew_if_required() {
    `homebrew_exists` && echo "Homebrew found" || bootstrap_homebrew
}

main() {
    case $OSTYPE in
        darwin*)
            install_homebrew_if_required
            install_ansible_if_required
            ;;
    esac
    provision "$@"
}

main "$@"
