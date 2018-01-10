#!/bin/bash

function nix_bootstrap
{
    # install nix
    curl https://nixos.org/nix/install -o nix_install.sh

    sh ./nix_install.sh

    # Stage nix configuraiton
    sudo mkdir -p /etc/nix
    sudo cp /src/vagrant/nix.conf /etc/nix/nix.conf

    # Load profile
    . /home/vagrant/.nix-profile/etc/profile.d/nix.sh

    # Install cabal tools
    nix-env -i cabal-install
    nix-env -i cabal2nix
}

function os_bootstrap
{
    # Enable EPEL
    sudo yum install -y epel-release

    # Install standard tooling
    # sudo yum group install -y "Development Tools"
    sudo yum install -y bind-utils
    sudo yum install -y nmap-ncat

    # Setup bash completion
    sudo yum install -y bash-completion bash-completion-extras
}

nix_bootstrap
os_bootstrap
