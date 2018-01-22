clean:
	rm -rf dist

packages-nix:
	cabal2nix . > packages.nix

cabal-update:
	nix-shell --run "cabal update"

build:
	nix-shell --run "cabal configure --ghcjs --ghcjs-options='-Werror'; cabal build -v"

cp-site:
	rm -rf dist-site
	mkdir dist-site
	cp -r ./dist/build/chartjs-exe/chartjs-exe.jsexe/* dist-site/
	cp -rf ./static/* dist-site/
	cp ./raw_html/index.html dist-site/

build-site: clean packages-nix cabal-update build cp-site

serve-site:
	cd dist-site && python -m SimpleHTTPServer 8888
