
all: build

tezos/Makefile:
	git clone git@gitlab.com:tezos/tezos.git

build: _obuild tezos/Makefile
	ocp-build build

install: _obuild
	ocp-build install

_obuild: Makefile
	ocp-build init

clean-tests:
	rm -f tests/*.liq.tz \
		tests/*.liq.debug \
		tests/*.liq.tz.liq \
		tests/*.pdf \
		tests/*.dot \
		tests/*.syntax \
		tests/*.typed \
		tests/*.mic \
		tests/*.normal \
		tests/*.pre \
		tests/others/*.*.*
	rm -f *~ tests/*~ tests/others/*~

clean-sources:
	rm -f tools/*/*~ libs/*/*~

clean: _obuild clean-tests clean-sources
	ocp-build clean

distclean: clean
	rm -rf _obuild



NTESTS=18
SIMPLE_TESTS= `seq -f 'test%.0f' 0 $(NTESTS)`
MORE_TESTS=test_ifcons test_if test_loop test_option test_transfer test_left
OTHER_TESTS=others/broker others/demo
REV_TESTS=`seq -f  'test%.0f' 0 5`

TEZOS_TESTS=and exec_concat max_in_list steps_to_quota balance		\
fail_amount noop store_input build_list fail not store_now		\
check_signature first originator str_id compare get_map_value or	\
swap_left_right concat_hello hardlimit pair_id tez_add_sub		\
concat_list hash_string ret_int transfer_amount contains_all if		\
reverse_loop transfer_to create_account list_id_map reverse		\
weather_insurance create_contract list_id set_id xor default_account	\
map_id set_member empty_map map_size set_size

#EXIT_ON_ERROR= || exit 2
EXIT_ON_ERROR= || echo Test $$i failed
tests: build
	for i in $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check.sh $$i $(EXIT_ON_ERROR); \
	done

rev-tests: build
	for i in $(REV_TESTS); do \
		./check-rev.sh tests/reverse $$i $(EXIT_ON_ERROR); \
	done
	for i in $(TEZOS_TESTS); do \
		./check-rev.sh tezos/test/contracts $$i $(EXIT_ON_ERROR); \
	done


