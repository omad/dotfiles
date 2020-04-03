
function __fish_airflow_list_dags
    # print everything after the second line of ------------
    airflow list_dags | awk 'BEGIN {seenlines=0} NF==1 {if (seenlines>=2) print} /-------/ {seenlines++}'
end

function __fish_airflow_list_tasks
    set -l cmd (commandline -opc)
    airflow list_tasks $cmd[-1] | awk 'NF==1'

end
function __fish_airflow_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'airflow' ]
    return 0
  end
  return 1
end

function __fish_airflow_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end


# Erase any existing completion
complete -c airflow -e

# No filenames, -h or --help anywhere
complete -c airflow -f -s h -l help

set -l airflow_commands backfill list_dag_runs list_tasks clear pause unpause trigger_dag delete_dag show_dag pool variables kerberos render run initdb list_dags dag_state task_failed_deps task_state serve_logs test webserver resetdb upgradedb checkdb shell scheduler worker flower version connections create_user delete_user list_users sync_perm next_execution rotate_fernet_key

complete -c airflow -f -n "not __fish_seen_subcommand_from $airflow_commands" -a "$airflow_commands"

# List Tasks
complete -c airflow -f -n "__fish_seen_subcommand_from list_tasks; and not __fish_seen_subcommand_from (__fish_airflow_list_dags)" -a "(__fish_airflow_list_dags)"
complete -c airflow -f -n "__fish_seen_subcommand_from list_tasks" -s t -l tree -d "Tree view"

# Run Task
set -l task_commands test run task_state
complete -c airflow -f -n "__fish_seen_subcommand_from $task_commands; and __fish_is_token_n 3" -a "(__fish_airflow_list_dags)"
complete -c airflow -f -n "__fish_seen_subcommand_from $task_commands; and __fish_is_token_n 4" -a "(__fish_airflow_list_tasks)"



