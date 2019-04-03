# This script is used in .travis.yml for continuous integration on travis.
# This also show some needed system packages to build liquidity.
# Travis CI is done on Ubuntu Xenial

sudo apt-get update -qq
sudo apt-get install -y -qq libgmp-dev

# do this in a second step
# for ubuntu, these packages are too old or not available in xenial
sudo add-apt-repository "deb http://fr.archive.ubuntu.com/ubuntu artful main universe"
sudo apt-get update -qq
sudo apt-get install -y -qq \
     libsecp256k1-dev libsecp256k1-0 libsodium-dev libssl-dev \
     bubblewrap libev-dev libhidapi-dev
