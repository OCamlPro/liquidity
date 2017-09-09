
all: build

build: _obuild
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
#EXIT_ON_ERROR= || exit 2
tests: build
	for i in $(SIMPLE_TESTS) \
		$(MORE_TESTS) $(OTHER_TESTS); do \
		./check.sh $$i $(EXIT_ON_ERROR); \
	done


