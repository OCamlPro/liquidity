all:
	@echo 'Use "make clean" to clean this directory'

clean:
	for dir in . others reverse doc ../tezos/src/bin_client/test/contracts; do    \
		rm -f $$dir/*~;            \
		rm -f $$dir/*.cm?;         \
		rm -f $$dir/*.liq.tz;      \
		rm -f $$dir/*.init.tz;     \
		rm -f $$dir/*.initializer.tz;     \
		rm -f $$dir/*.liq.ocaml;   \
		rm -f $$dir/*.liq.simple;  \
		rm -f $$dir/*.liq.syntax;  \
		rm -f $$dir/*.liq.typed;   \
		rm -f $$dir/*.liq.pre;     \
		rm -f $$dir/*.liq.encoded; \
		rm -f $$dir/*.tz.liq;      \
		rm -f $$dir/*.tz.dot;      \
		rm -f $$dir/*.tz.pre;      \
		rm -f $$dir/*.tz.pdf;      \
	done
