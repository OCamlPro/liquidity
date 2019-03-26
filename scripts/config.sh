#!/bin/bash

export TEZOS_CLIENT_UNSAFE_DISABLE_DISCLAIMER=Y

TEZOS_FULL_PATH=$(which tezos-client || echo "./tezos/tezos-client")

DEFAULT='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
BOLD='\033[1m'
BRED='\033[1;31m'
BGREEN='\033[1;32m'

VERBOSE=

function run {
    TITLE=$1
    # printf "  %-40s" "$TITLE"
    printf "$TITLE:"
    CMD=$2
    if [[ $3 -eq 0 ]] || [[ -z $3 ]] ; then
        [[ ! -z "$VERBOSE" ]] && echo $CMD
        OUTPUT=$(eval $CMD 2>&1)
        STATUS=$?
        [[ ! -z "$VERBOSE" ]] && echo $OUTPUT
        if [[ "$STATUS" -eq 0 ]] ; then
            printf "${GREEN}[${BGREEN}OK${GREEN}]${DEFAULT} "
            # printf "     ${GREEN}[${BOLD}OK${DEFAULT}${GREEN}]${DEFAULT}\n"
        else
            printf "${RED}[${BRED}KO${RED}]${DEFAULT}\n"
            # printf "     ${RED}[${BOLD}KO${DEFAULT}${RED}]${DEFAULT}\n"
            printf -- "$BOLD---- Command ------------------------$DEFAULT\n"
            printf "$BOLD$CMD$DEFAULT\n"
            printf -- "$BOLD==== Output =========================$DEFAULT\n"
            echo -e "$OUTPUT\n"
            printf -- "$BOLD=====================================$DEFAULT\n"
            exit 2
        fi
    else
        printf "${BLUE}[SKIPPED]${DEFAULT} "
        # printf "${BLUE}[SKIPPED]${DEFAULT}\n"
    fi
}
