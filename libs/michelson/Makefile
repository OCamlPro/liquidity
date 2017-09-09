
TEZOS_DIR=$(HOME)/GIT/gitlab/tezos/src
PROTO_DIR=$(TEZOS_DIR)/proto/alpha
CLIENT_DIR=$(TEZOS_DIR)/client/embedded/alpha
MINUTILS_DIR=$(TEZOS_DIR)/minutils
UTILS_DIR=$(TEZOS_DIR)/utils
NODE_DIR=$(TEZOS_DIR)/node/db

MINUTILS_FILES= \
        data_encoding.ml   	data_encoding.mli \

UTILS_FILES= \
        error_monad_sig.ml    	\
        error_monad.ml  	error_monad.mli  \
        base58.ml  		base58.mli  \
        hash.ml  		hash.mli  \
	time.ml

#NODE_FILES=\
#        persist.ml    		persist.mli \

PROTO_FILES= \
        tezos_hash.ml    	\
	\
        qty_repr.ml    		\
        tez_repr.ml    		\
	\
        script_int_repr.ml 	script_int_repr.mli   \
        script_repr.ml  	script_repr.mli  \
        contract_repr.ml  	contract_repr.mli  \
        period_repr.ml    	\
        constants_repr.ml    	\
        seed_repr.ml   		seed_repr.mli \
        roll_repr.ml   		roll_repr.mli \
        cycle_repr.ml    	cycle_repr.mli \
        raw_level_repr.ml    	raw_level_repr.mli \
        voting_period_repr.ml   voting_period_repr.mli \
        level_repr.ml  		level_repr.mli  \
        fitness_repr.ml    	\
        vote_repr.ml    	vote_repr.mli\
        time_repr.ml    	\
	\
        storage_sigs.ml    	\
        storage_functors.ml  	storage_functors.mli  \
        storage.ml    		\
        level_storage.ml  	level_storage.mli  \
        seed_storage.ml    	seed_storage.mli \
        roll_storage.ml    	roll_storage.mli \
        reward_storage.ml    	reward_storage.mli \
        bootstrap_storage.ml    bootstrap_storage.mli \
        vote_storage.ml    	vote_storage.mli \
	\
        contract_storage.ml  	contract_storage.mli  \
        \
        \
        script_typed_ir.ml      \
        script_ir_translator.ml \
        script_interpreter.ml 	script_interpreter.mli \
        init_storage.ml

CLIENT_FILES=\
          script_located_ir.ml  \
          concrete_lexer.mll    \
          concrete_parser.mly   \
          client_proto_programs.ml  client_proto_programs.mli  \


FILES= \
  $(patsubst %,$(PROTO_DIR)/%,$(PROTO_FILES)) \
  $(patsubst %,$(CLIENT_DIR)/%,$(CLIENT_FILES)) \
  $(patsubst %,$(MINUTILS_DIR)/%,$(MINUTILS_FILES)) \
  $(patsubst %,$(UTILS_DIR)/%,$(UTILS_FILES)) \
  $(patsubst %,$(NODE_DIR)/%,$(NODE_FILES)) \

all:
	@echo 'Use "make copy" to update Tezos files'

copy:
	cp $(FILES) .

