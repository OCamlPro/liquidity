
all: build

clone-tezos:
	git submodule update --init

build: _obuild
	ocp-build build 
	cp -f _obuild/liquidity-mini/liquidity-mini.asm ./liquidity-mini
	cp -f _obuild/liquidity/liquidity.asm ./liquidity

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
	opam install ocp-build ocplib-endian zarith calendar digestif.0.7 hex ocurl lwt \
	       lwt_log uri sodium bigstring ezjsonm menhir

distclean: clean
	rm -rf _obuild

doc:
	$(MAKE) -C docs/sphinx/

# All of these tests must be run with with_tezos=true

NTESTS=42
NREVTESTS=9
SIMPLE_TESTS=`seq -f 'test%.0f' 0 $(NTESTS)`
MORE_TESTS=test_ifcons test_if test_loop test_option test_transfer test_call test_left \
  test_extfun test_left_constr test_closure test_closure2 test_closure3 \
  test_map test_rev test_reduce_closure test_map_closure test_mapreduce_closure \
  test_mapmap_closure test_setreduce_closure test_left_match test_loop_left \
  test_fold test_iter test_big_map test_map_fold_closure test_inline test_rec_fun \
  bug_annot0 inline_fail bug_annot1 test_infer_unpack test_infer_param test_record
OTHER_TESTS=others/broker others/demo others/auction others/multisig others/alias others/game others/mist_wallet_current others/token
DOC_TESTS=`cd tests; find doc -regex "[^\.]+.liq" -exec sh -c "echo {} | cut -d '.' -f 1" \; | sort -V`
REV_TESTS=`seq -f 'test%.0f' 0 $(NREVTESTS)`

NEW_TEZOS_TESTS= fail weather_insurance
FAILING_TEZOS_TESTS= originator
TEZOS_TESTS=accounts add1_list add1 add_delta_timestamp add_timestamp_delta after_strategy always and append assert_cmpeq assert_cmpge assert_cmpgt assert_cmple assert_cmplt assert_cmpneq assert_eq assert_ge assert_gt assert_le assert_lt assert_neq assert at_least auction bad_lockup balance big_map_get_add big_map_mem big_map_union build_list cadr_annotation check_signature compare concat_list concat conditionals cons_twice contains_all cps_fact create_account create_add1_lists create_contract data_publisher default_account diff_timestamps dispatch empty_map empty exec_concat fail_amount fail faucet first forward get_map_value hardlimit hash_consistency_checker hash_key hash_string id if_some if infinite_loop insertion_sort int_publisher king_of_tez list_id_map list_id list_iter2 list_iter list_map_block list_of_transactions lockup macro_annotations loop_left map_caddaadr map_car map_id map_iter map_size max_in_list min noop not originator or packunpack pair_id pair_macro parameterized_multisig queue reduce_map reentrancy replay reservoir ret_int reveal_signed_preimage reverse_loop reverse scrutable_reservoir self set_caddaadr set_car set_cdr set_id set_iter set_member set_size spawn_identities steps_to_quota store_input store_now str_id subset sub_timestamp_delta swap_left_right take_my_money tez_add_sub transfer_amount transfer_to unpair_macro vote_for_delegate weather_insurance xor

EXIT_ON_ERROR= || exit 2
#EXIT_ON_ERROR= || echo Test $$i failed
tests: build
	for i in $(DOC_TESTS) $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check.sh $$i $(EXIT_ON_ERROR); \
	done

tests-mini: build
	for i in $(DOC_TESTS) $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check-mini.sh $$i $(EXIT_ON_ERROR); \
	done

rev-tests: build
	for i in $(REV_TESTS); do \
		./check-rev.sh tests/reverse $$i $(EXIT_ON_ERROR); \
	done
	for i in $(TEZOS_TESTS); do \
		./check-rev.sh tezos/src/bin_client/test/contracts/ $$i $(EXIT_ON_ERROR); \
	done

all-tests: tests tests-mini rev-tests
