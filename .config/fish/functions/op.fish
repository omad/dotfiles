function op --wraps=op
    if test (count argv) -ge 1; and test $argv[1] = "signin";
        set -Ux OP_SESSION_my (command op signin my --raw)
    else
        command op $argv
    end

end

