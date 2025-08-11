function _llm_completion
    set -l response (env _LLM_COMPLETE=fish_complete COMP_WORDS=(commandline -cp) COMP_CWORD=(commandline -t) llm)
    for completion in $response
        set -l metadata (string split "," $completion)
        if test $metadata[1] = dir
            __fish_complete_directories $metadata[2]
        else if test $metadata[1] = file
            __fish_complete_path $metadata[2]
        else if test $metadata[1] = plain
            echo $metadata[2]
        end
    end
end

function _llm_models_completion
    # Run 'llm models' and process its output
    llm models | perl -lne 'print $1 if /: ([^ ]+)/'
end

complete -ec llm
# Set up the autocompletion
complete -c llm --short-option m --long-option model --arguments "(_llm_models_completion)" --require-parameter --no-files -d "LLM model to use"

# complete --no-files --command llm --arguments "(_llm_completion)"