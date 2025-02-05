function jpp --wraps='pre-commit run -a; and jj git push' --description 'alias jpp=pre-commit run -a; and jj git push'
  pre-commit run -a; and jj git push $argv
        
end
