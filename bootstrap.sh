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
    ansible-playbook -i hosts provisioning/site.yml --ask-become-pass "$@"
}

install_ansible_if_required() {
    `ansible_exists` && echo "Ansible found" || bootstrap_ansible
}

install_homebrew_if_required() {
    `homebrew_exists` && echo "Homebrew found" || bootstrap_homebrew
}

main() {
    install_homebrew_if_required
    install_ansible_if_required
    provision "$@"
}

main "$@"
