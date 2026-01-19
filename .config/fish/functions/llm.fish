function llm
  # Inspirationhttps://notes.billmill.org/dev_blog/2026/01/mdstream_-_a_vibecoding_experiment.html

  if type -q mdriver
    command llm $argv | mdriver
  else if type -q bat
    command llm $argv | bat --paging=never --style=plain --language=markdown
  else
    command llm $argv
  end

end
