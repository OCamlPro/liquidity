
all: build

clone-tezos:
	git clone -b alphanet https://github.com/tezos/tezos.git # clone with https

build: _obuild
	ocp-build build

install: _obuild
	ocp-build install

_obuild: Makefile
	ocp-build init

clean-tests:
	$(MAKE) -C tests clean

clean-sources:
	rm -f tools/*/*~ libs/*/*~

clean: _obuild clean-tests clean-sources
	ocp-build clean

distclean: clean
	rm -rf _obuild

# All of these tests must be run with with_tezos=true

NTESTS=32
SIMPLE_TESTS= `seq -f 'test%.0f' 0 $(NTESTS)`
MORE_TESTS=test_ifcons test_if test_loop test_option test_transfer test_left \
  test_extfun test_left_constr test_closure test_closure2 test_closure3 \
  test_map test_rev test_reduce_closure test_map_closure test_mapreduce_closure \
  test_mapmap_closure test_setreduce_closure test_left_match
OTHER_TESTS=others/broker others/demo others/auction
REV_TESTS=`seq -f  'test%.0f' 0 5`

NEW_TEZOS_TESTS= fail weather_insurance
FAILING_TEZOS_TESTS= originator
TEZOS_TESTS=and exec_concat max_in_list steps_to_quota balance		\
fail_amount noop store_input build_list not store_now		\
check_signature first str_id compare get_map_value or	\
swap_left_right concat_hello hardlimit pair_id tez_add_sub		\
concat_list hash_string ret_int transfer_amount contains_all if		\
reverse_loop transfer_to create_account list_id_map reverse		\
 create_contract list_id set_id xor default_account	\
map_id set_member empty_map map_size set_size

EXIT_ON_ERROR= || exit 2
#EXIT_ON_ERROR= || echo Test $$i failed
tests: build
	for i in $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check.sh $$i $(EXIT_ON_ERROR); \
	done

tests-mini: build
	for i in $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check-mini.sh $$i $(EXIT_ON_ERROR); \
	done

rev-tests: build
	for i in $(REV_TESTS); do \
		./check-rev.sh tests/reverse $$i $(EXIT_ON_ERROR); \
	done
	for i in $(TEZOS_TESTS); do \
		./check-rev.sh tezos/test/contracts $$i $(EXIT_ON_ERROR); \
	done


