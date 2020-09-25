
abbr --add g 'git'
abbr --add y 'yadm'
abbr --add hm home-manager

abbr --add kc kubectl

# Git shortcuts
abbr --add ga 'git add'
abbr --add gaa 'git add .'
abbr --add gb 'git branch'
abbr --add gba 'git branch -a'
abbr --add gbd 'git branch -D'
abbr --add gbl 'git branch | grep "*" | sed "s/* //"'
abbr --add gc 'git commit'
abbr --add gca 'git commit -a'
abbr --add gcd 'cd (git rev-parse --show-toplevel)'
abbr --add gco 'git checkout'
abbr --add gcob 'git checkout -b'
abbr --add gcod 'git checkout development'
abbr --add gcom 'git checkout master'
abbr --add gcp 'git cherry-pick'
abbr --add gcs 'git commit -sS'
abbr --add gd 'git diff'
abbr --add gfp 'git fetch --prune'
abbr --add gi 'git ignore'
abbr --add gl "git log --pretty='format:%C(Yellow)%h%Creset %C(Blue)%ar%Creset %an - %s' --graph"
abbr --add gm 'git merge --no-ff'
abbr --add gpoh 'git push origin HEAD'
abbr --add gpull 'git pull'
abbr --add gpush 'git push'
abbr --add grh 'git reset --hard'
abbr --add gst 'git status'
abbr --add gt 'git tag'
abbr --add gts 'git tag -s'
abbr --add lg 'lazygit'
abbr --add ts 'tig status'

# yadm shortcuts
abbr --add ya 'yadm add'
abbr --add yc 'yadm commit'
abbr --add yd 'yadm diff'
abbr --add yst 'yadm status'

abbr --add mkdir 'mkdir -pv'

abbr --add qsubi qsub -I -q express -l wd,walltime=2:00:00,mem=7GB,ncpus=2 -l storage=gdata/u46+gdata/v10+gdata/fk4+gdata/rs0+gdata/if87 -P u46
