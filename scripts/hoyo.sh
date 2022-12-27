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

_hoyo()
{
    local CMDLINE
    local IFS=$'\n'
    CMDLINE=(--bash-completion-index $COMP_CWORD)

    for arg in ${COMP_WORDS[@]}; do
        CMDLINE=(${CMDLINE[@]} --bash-completion-word $arg)
    done

    COMPREPLY=( $(hoyo "${CMDLINE[@]}") )
}

complete -o filenames -F _hoyo hoyo
