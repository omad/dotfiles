# Fish shell completions for sops (https://github.com/getsops/sops)
#
# sops ships bash and zsh completions but not fish, so this fills the gap.
# Generated against sops 3.13.3, hand-tuned for fish.
#
# Install:
#   Copy or symlink this file into a directory on $fish_complete_path, e.g.
#     ln -s (pwd)/sops.fish ~/.config/fish/completions/sops.fish
#   then open a new shell (completions are loaded lazily on first use).

# ---------------------------------------------------------------------------
# State helpers
# ---------------------------------------------------------------------------

# The full set of top-level subcommands sops understands.
set -l __sops_cmds completion exec-env exec-file publish keyservice filestatus \
    groups updatekeys decrypt encrypt rotate edit set unset help

# True while the command line has no subcommand token yet. This is also the
# "implicit edit" form (`sops [flags] <file>`), which accepts the global flags.
# Any non-flag token (a filename, or a subcommand) turns this off, matching
# sops' own rule that flags must precede the filename.
function __fish_sops_needs_command
    set -l tokens (commandline -opc)
    set -e tokens[1]
    for t in $tokens
        string match -q -- '-*' $t; and continue
        return 1
    end
    return 0
end

# True when one of $argv subcommands is active, but not in a `sops help ...`
# context (so `sops help encrypt` doesn't surface encrypt's flags).
function __fish_sops_using
    __fish_seen_subcommand_from help; and return 1
    __fish_seen_subcommand_from $argv
end

# ---------------------------------------------------------------------------
# Value helpers
# ---------------------------------------------------------------------------

# File formats accepted by --input-type / --output-type.
function __sops_formats
    printf '%s\n' json yaml dotenv ini binary
end

# Key types accepted in a comma-separated --decryption-order list.
function __sops_key_types
    printf '%s\n' kms gcp-kms azure-kv hc-vault age pgp
end

# AWS profile names discovered from the local AWS config/credentials files.
function __sops_aws_profiles
    for f in ~/.aws/config ~/.aws/credentials
        test -r $f; or continue
        string match -rg '^\[(?:profile )?(.+)\]' < $f
    end | sort -u
end

# ---------------------------------------------------------------------------
# Reusable flag groups (registered against a given `-n` condition string)
# ---------------------------------------------------------------------------

# --input-type / --output-type
function __sops_reg_formats -a cond
    complete -c sops -n $cond -x -l input-type  -a '(__sops_formats)' -d 'Input format'
    complete -c sops -n $cond -x -l output-type -a '(__sops_formats)' -d 'Output format'
end

# Local + remote key service flags (shared by nearly every subcommand).
function __sops_reg_keyservice -a cond
    complete -c sops -n $cond -l enable-local-keyservice -d 'Use the local key service'
    complete -c sops -n $cond -x -l keyservice -d 'Extra key service, e.g. tcp://host:5000'
end

# --decryption-order
function __sops_reg_decorder -a cond
    complete -c sops -n $cond -x -l decryption-order -a '(__sops_key_types)' \
        -d 'Comma-separated decryption key types'
end

# Master-key selection flags used when encrypting new documents.
function __sops_reg_keys -a cond
    complete -c sops -n $cond -x -s k -l kms             -d 'Comma-separated KMS ARNs'
    complete -c sops -n $cond -x    -l aws-profile       -a '(__sops_aws_profiles)' -d 'AWS profile'
    complete -c sops -n $cond -x    -l gcp-kms           -d 'Comma-separated GCP KMS resource IDs'
    complete -c sops -n $cond -x    -l hckms             -d 'Comma-separated HuaweiCloud KMS key IDs'
    complete -c sops -n $cond -x    -l azure-kv          -d 'Comma-separated Azure Key Vault URLs'
    complete -c sops -n $cond -x    -l hc-vault-transit  -d 'Comma-separated Vault transit key URIs'
    complete -c sops -n $cond -x -s p -l pgp             -d 'Comma-separated PGP fingerprints'
    complete -c sops -n $cond -x -s a -l age             -d 'Comma-separated age recipients'
end

# add-*/rm-* master-key management flags (top-level and `rotate`).
function __sops_reg_masterkeys -a cond
    complete -c sops -n $cond -x -l add-kms  -d 'Add KMS ARNs to master keys'
    complete -c sops -n $cond -x -l rm-kms   -d 'Remove KMS ARNs from master keys'
    complete -c sops -n $cond -x -l add-pgp  -d 'Add PGP fingerprints to master keys'
    complete -c sops -n $cond -x -l rm-pgp   -d 'Remove PGP fingerprints from master keys'
    complete -c sops -n $cond -x -l add-age  -d 'Add age recipients to master keys'
    complete -c sops -n $cond -x -l rm-age   -d 'Remove age recipients from master keys'
    complete -c sops -n $cond -x -l add-gcp-kms -d 'Add GCP KMS resource IDs to master keys'
    complete -c sops -n $cond -x -l rm-gcp-kms  -d 'Remove GCP KMS resource IDs from master keys'
    complete -c sops -n $cond -x -l add-azure-kv -d 'Add Azure Key Vault URLs to master keys'
    complete -c sops -n $cond -x -l rm-azure-kv  -d 'Remove Azure Key Vault URLs from master keys'
    complete -c sops -n $cond -x -l add-hckms -d 'Add HuaweiCloud KMS key IDs to master keys'
    complete -c sops -n $cond -x -l rm-hckms  -d 'Remove HuaweiCloud KMS key IDs from master keys'
    complete -c sops -n $cond -x -l add-hc-vault-transit -d 'Add Vault transit key URIs to master keys'
    complete -c sops -n $cond -x -l rm-hc-vault-transit  -d 'Remove Vault transit key URIs from master keys'
end

# ---------------------------------------------------------------------------
# Subcommands
# ---------------------------------------------------------------------------

complete -c sops -n __fish_sops_needs_command -f -a completion -d 'Generate shell completion scripts'
complete -c sops -n __fish_sops_needs_command -f -a exec-env   -d 'Run a command with decrypted values in the environment'
complete -c sops -n __fish_sops_needs_command -f -a exec-file  -d 'Run a command with decrypted contents as a temp file'
complete -c sops -n __fish_sops_needs_command -f -a publish    -d 'Publish a sops file/dir to a configured destination'
complete -c sops -n __fish_sops_needs_command -f -a keyservice -d 'Start a sops key service server'
complete -c sops -n __fish_sops_needs_command -f -a filestatus -d 'Report a file''s encryption status'
complete -c sops -n __fish_sops_needs_command -f -a groups     -d 'Modify the key groups on a sops file'
complete -c sops -n __fish_sops_needs_command -f -a updatekeys -d 'Update the keys of sops files from the config'
complete -c sops -n __fish_sops_needs_command -f -a decrypt    -d 'Decrypt a file to stdout'
complete -c sops -n __fish_sops_needs_command -f -a encrypt    -d 'Encrypt a file to stdout'
complete -c sops -n __fish_sops_needs_command -f -a rotate     -d 'Rotate the data key and re-encrypt'
complete -c sops -n __fish_sops_needs_command -f -a edit       -d 'Edit an encrypted file'
complete -c sops -n __fish_sops_needs_command -f -a set        -d 'Set a key or branch in the document'
complete -c sops -n __fish_sops_needs_command -f -a unset      -d 'Unset a key or branch in the document'
complete -c sops -n __fish_sops_needs_command -f -a help       -d 'Show help for a command'

# `sops help <cmd>`
complete -c sops -n '__fish_seen_subcommand_from help' -f -a "$__sops_cmds"

# `sops completion <shell>`
complete -c sops -n '__fish_sops_using completion; and not __fish_seen_subcommand_from bash zsh' \
    -f -a bash -d 'Generate bash completions'
complete -c sops -n '__fish_sops_using completion; and not __fish_seen_subcommand_from bash zsh' \
    -f -a zsh -d 'Generate zsh completions'

# `sops groups <add|delete>`
complete -c sops -n '__fish_sops_using groups; and not __fish_seen_subcommand_from add delete' \
    -f -a add -d 'Add a new key group'
complete -c sops -n '__fish_sops_using groups; and not __fish_seen_subcommand_from add delete' \
    -f -a delete -d 'Delete a key group by index'

# ---------------------------------------------------------------------------
# Global flags (implicit-edit form: `sops [flags] <file>`)
# ---------------------------------------------------------------------------

complete -c sops -n __fish_sops_needs_command -s d -l decrypt -d 'Decrypt a file to stdout'
complete -c sops -n __fish_sops_needs_command -s e -l encrypt -d 'Encrypt a file to stdout'
complete -c sops -n __fish_sops_needs_command -s r -l rotate  -d 'Rotate the data key and re-encrypt'
complete -c sops -n __fish_sops_needs_command -s i -l in-place -d 'Write result back to the file'
complete -c sops -n __fish_sops_needs_command -s s -l show-master-keys -d 'Show master keys while editing'
complete -c sops -n __fish_sops_needs_command -l ignore-mac -d 'Ignore the MAC during decryption'
complete -c sops -n __fish_sops_needs_command -l mac-only-encrypted -d 'Compute MAC only over encrypted values'
complete -c sops -n __fish_sops_needs_command -l verbose -d 'Enable verbose logging'
complete -c sops -n __fish_sops_needs_command -l disable-version-check -d 'Skip latest-version check on --version'
complete -c sops -n __fish_sops_needs_command -l check-for-updates -d 'Check for updates on --version'
complete -c sops -n __fish_sops_needs_command -s h -l help -d 'Show help'
complete -c sops -n __fish_sops_needs_command -s v -l version -d 'Print the version'

complete -c sops -n __fish_sops_needs_command -x -l extract -d 'Extract a key/branch, e.g. ["a"][0] (decrypt only)'
complete -c sops -n __fish_sops_needs_command -x -l set -d 'Set a key/branch (edit mode), e.g. ["a"] {"b":true}'
complete -c sops -n __fish_sops_needs_command -x -l encryption-context -d 'KMS encryption context key:value pairs'
complete -c sops -n __fish_sops_needs_command -x -l unencrypted-suffix -d 'Override the unencrypted key suffix'
complete -c sops -n __fish_sops_needs_command -x -l encrypted-suffix -d 'Override the encrypted key suffix'
complete -c sops -n __fish_sops_needs_command -x -l unencrypted-regex -d 'Only leave keys matching this regex unencrypted'
complete -c sops -n __fish_sops_needs_command -x -l encrypted-regex -d 'Only encrypt keys matching this regex'
complete -c sops -n __fish_sops_needs_command -x -l unencrypted-comment-regex -d 'Leave keys with matching comment unencrypted'
complete -c sops -n __fish_sops_needs_command -x -l encrypted-comment-regex -d 'Encrypt keys with matching comment'
complete -c sops -n __fish_sops_needs_command -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
complete -c sops -n __fish_sops_needs_command -x -l indent -d 'Spaces to indent YAML/JSON output'
complete -c sops -n __fish_sops_needs_command -r -l output -d 'Write output to this file'
complete -c sops -n __fish_sops_needs_command -r -l config -d 'Path to the sops config file'
complete -c sops -n __fish_sops_needs_command -r -l filename-override -d 'Filename to use for config/type detection'

__sops_reg_formats    __fish_sops_needs_command
__sops_reg_keys       __fish_sops_needs_command
__sops_reg_masterkeys __fish_sops_needs_command
__sops_reg_decorder   __fish_sops_needs_command
__sops_reg_keyservice __fish_sops_needs_command

# ---------------------------------------------------------------------------
# encrypt
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using encrypt' -s i -l in-place -d 'Write result back to the file'
complete -c sops -n '__fish_sops_using encrypt' -r -l output -d 'Write output to this file'
complete -c sops -n '__fish_sops_using encrypt' -x -l unencrypted-suffix -d 'Override the unencrypted key suffix'
complete -c sops -n '__fish_sops_using encrypt' -x -l encrypted-suffix -d 'Override the encrypted key suffix'
complete -c sops -n '__fish_sops_using encrypt' -x -l unencrypted-regex -d 'Only leave keys matching this regex unencrypted'
complete -c sops -n '__fish_sops_using encrypt' -x -l encrypted-regex -d 'Only encrypt keys matching this regex'
complete -c sops -n '__fish_sops_using encrypt' -x -l encryption-context -d 'KMS encryption context key:value pairs'
complete -c sops -n '__fish_sops_using encrypt' -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
complete -c sops -n '__fish_sops_using encrypt' -r -l filename-override -d 'Filename for config/type detection (required for stdin)'
__sops_reg_formats    '__fish_sops_using encrypt'
__sops_reg_keys       '__fish_sops_using encrypt'
__sops_reg_keyservice '__fish_sops_using encrypt'

# ---------------------------------------------------------------------------
# decrypt
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using decrypt' -s i -l in-place -d 'Write result back to the file'
complete -c sops -n '__fish_sops_using decrypt' -x -l extract -d 'Extract a key/branch, e.g. ["a"][0]'
complete -c sops -n '__fish_sops_using decrypt' -r -l output -d 'Write output to this file'
complete -c sops -n '__fish_sops_using decrypt' -l ignore-mac -d 'Ignore the MAC during decryption'
complete -c sops -n '__fish_sops_using decrypt' -r -l filename-override -d 'Filename for config/type detection'
__sops_reg_formats    '__fish_sops_using decrypt'
__sops_reg_decorder   '__fish_sops_using decrypt'
__sops_reg_keyservice '__fish_sops_using decrypt'

# ---------------------------------------------------------------------------
# edit
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using edit' -s s -l show-master-keys -d 'Show master keys while editing'
complete -c sops -n '__fish_sops_using edit' -l ignore-mac -d 'Ignore the MAC during decryption'
complete -c sops -n '__fish_sops_using edit' -x -l unencrypted-suffix -d 'Override the unencrypted key suffix'
complete -c sops -n '__fish_sops_using edit' -x -l encrypted-suffix -d 'Override the encrypted key suffix'
complete -c sops -n '__fish_sops_using edit' -x -l unencrypted-regex -d 'Only leave keys matching this regex unencrypted'
complete -c sops -n '__fish_sops_using edit' -x -l encrypted-regex -d 'Only encrypt keys matching this regex'
complete -c sops -n '__fish_sops_using edit' -x -l encryption-context -d 'KMS encryption context key:value pairs'
complete -c sops -n '__fish_sops_using edit' -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
__sops_reg_formats    '__fish_sops_using edit'
__sops_reg_keys       '__fish_sops_using edit'
__sops_reg_decorder   '__fish_sops_using edit'
__sops_reg_keyservice '__fish_sops_using edit'

# ---------------------------------------------------------------------------
# rotate
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using rotate' -s i -l in-place -d 'Write result back to the file'
complete -c sops -n '__fish_sops_using rotate' -r -l output -d 'Write output to this file'
complete -c sops -n '__fish_sops_using rotate' -x -l encryption-context -d 'KMS encryption context key:value pairs'
complete -c sops -n '__fish_sops_using rotate' -r -l filename-override -d 'Filename for config/type detection'
__sops_reg_formats    '__fish_sops_using rotate'
__sops_reg_masterkeys '__fish_sops_using rotate'
__sops_reg_decorder   '__fish_sops_using rotate'
__sops_reg_keyservice '__fish_sops_using rotate'

# ---------------------------------------------------------------------------
# set / unset
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using set' -r -l value-file -d 'Read the value from a file (avoids leaking in ps)'
complete -c sops -n '__fish_sops_using set' -l value-stdin -d 'Read the value from stdin'
complete -c sops -n '__fish_sops_using set' -l idempotent -d 'Do nothing if the index already has this value'
complete -c sops -n '__fish_sops_using set' -l ignore-mac -d 'Ignore the MAC during decryption'
complete -c sops -n '__fish_sops_using set' -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
__sops_reg_formats    '__fish_sops_using set'
__sops_reg_decorder   '__fish_sops_using set'
__sops_reg_keyservice '__fish_sops_using set'

complete -c sops -n '__fish_sops_using unset' -l idempotent -d 'Do nothing if the index does not exist'
complete -c sops -n '__fish_sops_using unset' -l ignore-mac -d 'Ignore the MAC during decryption'
complete -c sops -n '__fish_sops_using unset' -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
__sops_reg_formats    '__fish_sops_using unset'
__sops_reg_decorder   '__fish_sops_using unset'
__sops_reg_keyservice '__fish_sops_using unset'

# ---------------------------------------------------------------------------
# exec-env / exec-file
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using exec-env' -l background -d 'Background the process (DEPRECATED)'
complete -c sops -n '__fish_sops_using exec-env' -l pristine -d 'Only decrypted values in env, drop existing ones'
complete -c sops -n '__fish_sops_using exec-env' -l same-process -d 'Run in the current process, not a child'
complete -c sops -n '__fish_sops_using exec-env' -x -l user -a '(__fish_complete_users)' -d 'Run the command as this user'
__sops_reg_decorder   '__fish_sops_using exec-env'
__sops_reg_keyservice '__fish_sops_using exec-env'

complete -c sops -n '__fish_sops_using exec-file' -l background -d 'Background the process (DEPRECATED)'
complete -c sops -n '__fish_sops_using exec-file' -l no-fifo -d 'Use a regular file instead of a fifo'
complete -c sops -n '__fish_sops_using exec-file' -x -l user -a '(__fish_complete_users)' -d 'Run the command as this user'
complete -c sops -n '__fish_sops_using exec-file' -x -l filename -d 'Name for the temporary file'
__sops_reg_formats    '__fish_sops_using exec-file'
__sops_reg_decorder   '__fish_sops_using exec-file'
__sops_reg_keyservice '__fish_sops_using exec-file'

# ---------------------------------------------------------------------------
# publish
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using publish' -s y -l yes -d 'Pre-approve all changes, run non-interactively'
complete -c sops -n '__fish_sops_using publish' -l omit-extensions -d 'Omit file extensions in destination path'
complete -c sops -n '__fish_sops_using publish' -l recursive -d 'Publish a directory recursively'
complete -c sops -n '__fish_sops_using publish' -l verbose -d 'Enable verbose logging'
__sops_reg_decorder   '__fish_sops_using publish'
__sops_reg_keyservice '__fish_sops_using publish'

# ---------------------------------------------------------------------------
# updatekeys
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using updatekeys' -s y -l yes -d 'Pre-approve all changes, run non-interactively'
complete -c sops -n '__fish_sops_using updatekeys' -x -l input-type -a '(__sops_formats)' -d 'Input format'
__sops_reg_keyservice '__fish_sops_using updatekeys'

# ---------------------------------------------------------------------------
# filestatus
# ---------------------------------------------------------------------------
complete -c sops -n '__fish_sops_using filestatus' -x -l input-type -a '(__sops_formats)' -d 'Input format'

# ---------------------------------------------------------------------------
# groups add / delete
# ---------------------------------------------------------------------------
set -l __sops_groups_add '__fish_sops_using groups; and __fish_seen_subcommand_from add'
set -l __sops_groups_del '__fish_sops_using groups; and __fish_seen_subcommand_from delete'

complete -c sops -n $__sops_groups_add -r -s f -l file -d 'The file to add the group to'
complete -c sops -n $__sops_groups_add -s i -l in-place -d 'Write result back to the file'
complete -c sops -n $__sops_groups_add -x -l pgp -d 'PGP fingerprint for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l kms -d 'KMS ARN for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l aws-profile -a '(__sops_aws_profiles)' -d 'AWS profile'
complete -c sops -n $__sops_groups_add -x -l gcp-kms -d 'GCP KMS resource ID for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l hckms -d 'HuaweiCloud KMS key ID for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l azure-kv -d 'Azure Key Vault URL for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l hc-vault-transit -d 'Vault transit key URI for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l age -d 'age recipient for the new group (repeatable)'
complete -c sops -n $__sops_groups_add -x -l encryption-context -d 'KMS encryption context key:value pairs'
complete -c sops -n $__sops_groups_add -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
__sops_reg_keyservice $__sops_groups_add

complete -c sops -n $__sops_groups_del -r -s f -l file -d 'The file to delete the group from'
complete -c sops -n $__sops_groups_del -s i -l in-place -d 'Write result back to the file'
complete -c sops -n $__sops_groups_del -x -l shamir-secret-sharing-threshold -d 'Master keys needed to recover the data key'
__sops_reg_keyservice $__sops_groups_del
