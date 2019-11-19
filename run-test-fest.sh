#!/bin/bash

## run-test-fest.sh <major-number> <minor-number> <team-name> [<log-level> <other-flags>]
# Run a test fest for assignment <major-number>.<minor-number>
# for team <team-name>
# Optionally specify a certain log level
# Optionally specify other flags to provide to `ci-test-fest.rkt`

if [[ $1 == "" || $2 == "" || $3 == "" ]]; then
    printf "Error: Invalid usage.\n"
    printf "Usage: run-test-fest.sh <major-number> <minor-number> <team-name>\n"
    exit 1
fi

if [[ ! -d "./.git" && "$LOCAL_TEST_FEST" == "" ]]; then
    printf "Error: This script must be run at the root of your repository.\n"
    exit 1
fi

CWD=$(pwd)
ASSIGN_DIR="$CWD/Deliverables/$1/$1.$2"
ASSIGN_DIR_LOWER="$CWD/deliverables/$1/$1.$2"
LOG_LEVEL="$4"
OTHER_FLAGS="$5"

# Check directory structure
if [[ ! -d "$ASSIGN_DIR" && ! -d "$ASSIGN_DIR_LOWER" ]]; then
    printf "Error: Unable to locate deliverable folder at ${ASSIGN_DIR}.\n"
    printf "Is your directory structure correct?\n"
    exit 1
elif [[ -d "$ASSIGN_DIR_LOWER" ]]; then
    ASSIGN_DIR="$ASSIGN_DIR_LOWER"
fi

EXE_PATH="${ASSIGN_DIR}/run"

# Check exe
if [[ ! -f "$EXE_PATH" ]]; then
    printf "Error: makefile failed to build an executable,.\n"
    printf "or it isn't called `run`.\n"
    exit 1
fi

ORACLE_DIR=./oracle
if [[ "$LOCAL_TEST_FEST" == "" ]]; then
    # Setup environment
    source $ORACLE_DIR/ci-setup-racket.sh
    export PLTSTDERR="info@fest"
else
    export PLTSTDERR="debug@fest"
    ORACLE_DIR=../oracle
fi

if [[ "$LOG_LEVEL" != "" ]]; then
    export PLTSTDERR="$LOG_LEVEL@fest"
fi

printf "\nPath: %s\n\n" "$PATH"
type racket

if [[ -f ./ci-debug.sh ]]; then
    printf "Running ./ci-debug.sh...\n"
    source ./ci-debug.sh "$ORACLE_DIR"
fi

case "$1.$2" in
    "8.1")
	sudo ufw disable
	racket "$ORACLE_DIR"/test-fest/ci-admin-test.rkt -M $1 -m $2 -a "$EXE_PATH" $OTHER_FLAGS
	;;
    *)
	cd "$ORACLE_DIR" && racket test-fest/ci-test-fest.rkt -M $1 -m $2 -t "$EXE_PATH" -n $3 $OTHER_FLAGS
	;;
esac

