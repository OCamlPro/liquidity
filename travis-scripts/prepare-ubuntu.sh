############################################################################
#                               Liquidity                                  #
#                                                                          #
#                  Copyright (C) 2017-2019 OCamlPro SAS                    #
#                                                                          #
#                    Authors: Fabrice Le Fessant                           #
#                             Alain Mebsout                                #
#                             David Declerck                               #
#                                                                          #
#  This program is free software: you can redistribute it and/or modify    #
#  it under the terms of the GNU General Public License as published by    #
#  the Free Software Foundation, either version 3 of the License, or       #
#  (at your option) any later version.                                     #
#                                                                          #
#  This program is distributed in the hope that it will be useful,         #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of          #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           #
#  GNU General Public License for more details.                            #
#                                                                          #
#  You should have received a copy of the GNU General Public License       #
#  along with this program.  If not, see <https://www.gnu.org/licenses/>.  #
############################################################################

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
