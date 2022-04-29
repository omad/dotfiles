# granted fish shell completion

function __fish_granted_no_subcommand --description 'Test if there has been any subcommand yet'
    for i in (commandline -opc)
        if contains -- $i browser set settings print profile-order set completion token list clear uninstall help h
            return 1
        end
    end
    return 0
end

complete -c granted -n '__fish_granted_no_subcommand' -f -l verbose -d 'Log debug messages'
complete -c granted -n '__fish_granted_no_subcommand' -f -l help -s h -d 'show help'
complete -c granted -n '__fish_granted_no_subcommand' -f -l version -s v -d 'print the version'
complete -c granted -n '__fish_granted_no_subcommand' -f -l help -s h -d 'show help'
complete -c granted -n '__fish_granted_no_subcommand' -f -l version -s v -d 'print the version'
complete -c granted -n '__fish_seen_subcommand_from browser' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'browser' -d 'View the web browser that Granted uses to open cloud consoles'
complete -c granted -n '__fish_seen_subcommand_from set' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from browser' -a 'set' -d 'Change the web browser that Granted uses to open cloud consoles'
complete -c granted -n '__fish_seen_subcommand_from set' -f -l browser -s b -r -d 'Specify a default browser without prompts, e.g `-b firefox`, `-b chrome`'
complete -c granted -n '__fish_seen_subcommand_from set' -f -l path -s p -r -d 'Specify a path to the browser without prompts, requires -browser to be provided'
complete -c granted -n '__fish_seen_subcommand_from settings' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'settings' -d 'Manage Granted settings'
complete -c granted -n '__fish_seen_subcommand_from print' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from settings' -a 'print' -d 'List Granted Settings'
complete -c granted -n '__fish_seen_subcommand_from profile-order' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from settings' -a 'profile-order' -d 'Update profile ordering when assuming'
complete -c granted -n '__fish_seen_subcommand_from set' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from profile-order' -a 'set' -d 'Sets the method of ordering IAM profiles in the assume method'
complete -c granted -n '__fish_seen_subcommand_from completion' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'completion' -d 'Add autocomplete to your granted cli installation'
complete -c granted -n '__fish_seen_subcommand_from completion' -f -l shell -s s -r -d 'Shell type to generate completion for (fish)'
complete -c granted -n '__fish_seen_subcommand_from completion' -f -l help -s h -d 'show help'
complete -c granted -n '__fish_seen_subcommand_from token' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'token' -d 'Manage aws access tokens'
complete -c granted -n '__fish_seen_subcommand_from list' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from token' -a 'list' -d 'Lists all access tokens saved in the keyring'
complete -c granted -n '__fish_seen_subcommand_from clear' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_seen_subcommand_from token' -a 'clear' -d 'Remove a selected token from the keyring'
complete -c granted -n '__fish_seen_subcommand_from clear' -f -l all -s a -d 'Remove all saved tokens from keyring'
complete -c granted -n '__fish_seen_subcommand_from uninstall' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'uninstall' -d 'Remove all Granted configuration'
complete -c granted -n '__fish_seen_subcommand_from help h' -f -l help -s h -d 'show help'
complete -r -c granted -n '__fish_granted_no_subcommand' -a 'help h' -d 'Shows a list of commands or help for one command'

# assume fish shell completion

function __fish_assume_no_subcommand --description 'Test if there has been any subcommand yet'
    for i in (commandline -opc)
        if contains -- $i
            return 1
        end
    end
    return 0
end

complete -c assume -n '__fish_assume_no_subcommand' -f -l console -s c -d 'Open a web console to the role'
complete -c assume -n '__fish_assume_no_subcommand' -f -l unset -s un -d 'Unset all environment variables configured by Assume'
complete -c assume -n '__fish_assume_no_subcommand' -f -l url -s u -d 'Get an active console session url'
complete -c assume -n '__fish_assume_no_subcommand' -f -l service -s s -r -d 'Specify a service to open the console into'
complete -c assume -n '__fish_assume_no_subcommand' -f -l region -s r -r -d 'Specify a region to open the console into'
complete -c assume -n '__fish_assume_no_subcommand' -f -l pass-through -s pt -r -d 'Pass args to proxy assumer'
complete -c assume -n '__fish_assume_no_subcommand' -f -l active-role -s ar -d 'Open console using active role'
complete -c assume -n '__fish_assume_no_subcommand' -f -l verbose -d 'Log debug messages'
complete -c assume -n '__fish_assume_no_subcommand' -f -l auto-configure-shell -d 'Configure shell alias without prompts'
complete -c assume -n '__fish_assume_no_subcommand' -f -l exec -r -d 'assume a profile then execute this command'
complete -c assume -n '__fish_assume_no_subcommand' -f -l duration -s d -r -d 'Set session duration for your assumed role'
complete -c assume -n '__fish_assume_no_subcommand' -f -l help -s h -d 'show help'
complete -c assume -n '__fish_assume_no_subcommand' -f -l version -s v -d 'print the version'
