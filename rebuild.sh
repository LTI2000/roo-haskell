. ./.env
cabal clean
./generate.sh
cabal build
cabal test
