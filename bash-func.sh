#!/usr/bin/env bash

function hoyo {
    IFS=

    stdout="$(hoyo-cli $@)"
    exit_code=$?

    if [ $exit_code -eq 3 ]
    then
        eval $stdout
    elif [ ! -z "$stdout" ]
    then
        echo $stdout
    fi

    return $exit_code
}
