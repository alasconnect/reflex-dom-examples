## Build Environment

We maintain a vagrant configuration for a standard Dev/Build environment at the root of the repository. This is used to abstract away your host OS so you can run whatever you want/need and use your favorite editor, and be able to build and run the examples in a sane linux dev environment.

To use this environment, you will need [Vagrant](https://www.vagrantup.com/) and [Virtualbox](https://www.virtualbox.org/wiki/Downloads).
On Windows, you will need [git for windows](https://git-scm.com/download/win), with SSH in your PATH in your powershell profile.

The repository is mounted in the vm under `/src/`.

Note: When your host OS is windows, you should ensure that git is set to checkout as is, commit unix line endings (We want to be working with LF, not CRLF).

## Important Submodule Detail

The first time you check out the project, you will have to initialize git submodules:
+ `git submodule init`
+ `git submodule update`

## Getting Started Example

+ `vagrant up`
+ `vagrant ssh`
+ `cd /src/base`
+ `make build-site`
+ `make serve-site`
+ On the host machine visit: `http://172.28.128.12:8888`

## Creating a new example

+ Create a folder for your new example at the root of this project.
+ Copy the contents of `base` into the new folder.
+ Delete `packages.nix` since it is generated for us by `make build-site`.
+ Replace all instances of `rfex-base` in the following files with the name you'd like for your example.
  + default.nix
  + main.cabal
  + Makefile

## Building and running an example

Each example is its own bubble. The only common thread is a dependency on the root level submodule for reflex platform. By default the following steps should allow you to build and serve an example. In the event that the `Makefile` for the example has been altered you'll need to look at the individual example's `README.md` for details on building and running.

+ `cd /src/THE_EXAMPLE`
+ `make build-site`
+ `make serve-site`
+ On the host machine visit: `http://172.28.128.12:8888`
