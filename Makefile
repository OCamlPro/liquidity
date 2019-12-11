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

all: build

clone-dune-network:
	git submodule update --init

_obuild/liquidity/liquidity.asm: _obuild
	ocp-build build liquidity

_obuild/liquidity-mini/liquidity-mini.asm: _obuild
	ocp-build build liquidity-mini

_obuild/liquidity-client/liquidity-client.asm: _obuild
	ocp-build build liquidity-client

liquidity-client: _obuild/liquidity-client/liquidity-client.asm
	cp -f _obuild/liquidity-client/liquidity-client.asm liquidity-client

_obuild/makelove/makelove.asm: _obuild
	ocp-build build makelove

liquidity-mini: _obuild/liquidity-mini/liquidity-mini.asm
	cp -f _obuild/liquidity-mini/liquidity-mini.asm liquidity-mini

liquidity: _obuild/liquidity/liquidity.asm
	cp -f _obuild/liquidity/liquidity.asm liquidity


client: liquidity-mini
mini: liquidity-mini
build: liquidity liquidity-mini liquidity-client

makelove: _obuild/makelove/makelove.asm
	cp -f _obuild/makelove/makelove.asm makelove

install: _obuild
	ocp-build install liquidity

_obuild: Makefile
	ocp-build init

clean-tests:
	$(MAKE) -C tests clean

clean-sources:
	rm -f tools/*/*~ libs/*/*~

clean: _obuild clean-tests clean-sources
	ocp-build clean
	rm -f liquidity

build-deps:
	opam install . --deps-only --working-dir -y

distclean: clean
	rm -rf _obuild

doc:
	$(MAKE) -C docs/sphinx/

headers:
	headache -h misc/header -c misc/headache_config \
		Makefile build.ocp2 \
		scripts/*.sh travis-scripts/*.sh \
                tools/liquidity/build.ocp2 \
		tools/liquidity/*.ml[ily] \
		tools/liquidity/*.ml \
		tools/liquidity/with*-dune-network/*.ml[ily] \
		tools/liquidity/with*-dune-network/*.ml \
		tools/liquidity/reason/liquidReasonParse.ml \
		libs/*/build.ocp2 \
		libs/*/*.ml[ily] \
		libs/*/*.ml \
		libs/*/*/*.ml


# All of these tests must be run with with_dune_network=true

NTESTS=43
NREVTESTS=10
SIMPLE_TESTS=`seq -f 'test%.0f.liq' 0 $(NTESTS)`
MORE_TESTS=test_ifcons test_if test_loop test_option test_transfer test_call test_left \
  test_extfun test_left_constr test_closure test_closure2 test_closure3 \
  test_map test_rev test_reduce_closure test_map_closure test_mapreduce_closure \
  test_mapmap_closure test_setreduce_closure test_left_match test_loop_left \
  test_fold test_iter test_big_map test_map_fold_closure test_inline test_rec_fun \
  bug_annot0 inline_fail bug_annot1 test_infer_unpack test_infer_param test_record \
  bug187 test_modules lambda_const votes bug_197 curry bug_210 bug_213 \
  bug_214 bug_216 bug_steven1 bug_steven2 bug_inline2
RE_TESTS=bug202
OTHER_TESTS=others/broker others/demo others/auction others/multisig others/alias others/game others/mist_wallet_current others/token others/token_vote others/token_no_fee others/new_token
DOC_TESTS=`cd tests; find doc -regex "[^\.]+.liq" | sort -V`
REV_TESTS=`find tests/reverse -regex "[^\.]+.tz" | sort -V`

NEW_DUNE_TESTS= fail weather_insurance
FAILING_DUNE_TESTS= originator
DUNE_TESTS=`find dune-network/src/bin_client/test/contracts -regex "[^\.]+.tz" \
            ! -path "*concat_hello.tz" \
            ! -path "*/deprecated/*" \
            ! -path "*pexec*.tz" \
            | sort -V`

TESTS=$(DOC_TESTS) $(SIMPLE_TESTS) $(MORE_TESTS:=.liq) $(RE_TESTS:=.reliq) $(OTHER_TESTS:=.liq)

tests: build
	@echo ---------------------
	@echo Run full tests
	@echo ---------------------
	@scripts/run-list.sh scripts/check.sh $(TESTS)

tests-mini: mini
	@echo ---------------------
	@echo Run mini tests
	@echo ---------------------
	@scripts/run-list.sh scripts/check-mini.sh $(TESTS)

RTESTS=$(REV_TESTS) $(DUNE_TESTS)

rev-tests: build
	@echo ---------------------
	@echo Run reverse tests
	@echo ---------------------
	@scripts/run-list.sh scripts/check-rev.sh $(RTESTS)

all-tests: tests tests-mini rev-tests
