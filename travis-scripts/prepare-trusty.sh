# This script is used in .travis.yml for continuous integration on travis.
# BTW, it also show some needed system packages to build liquidity.
# Travis CI is done on Ubuntu trusty

sudo apt-get update -qq
sudo apt-get install -y -qq libgmp-dev pandoc # ocaml ocaml-native-compilers

# do this in a second step to only install libsecp256k1-dev libsecp256k1-0
# for ubuntu, these packages are not available in trusty
sudo add-apt-repository "deb http://archive.ubuntu.com/ubuntu artful main universe"
sudo apt-get update -qq
sudo apt-get install -y -qq \
     libsecp256k1-dev libsecp256k1-0 libsodium-dev libssl-dev \
     bubblewrap libev-dev libhidapi-dev
