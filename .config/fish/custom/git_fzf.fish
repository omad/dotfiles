# @ttscoff's version of @max-sixty's revision of @aluxian's
# fish translation of @junegunn's fzf git keybindings
# https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
# https://gist.github.com/aluxian/9c6f97557b7971c32fdff2f2b1da8209

# Deciphered from fzf-file-widget. Somewhat unclear why it doesn't exist already!
function fzf_add_to_commandline -d 'add stdin to the command line, for fzf functions'
	read -l result
	commandline -t ""
	commandline -it -- (string escape $result)
	commandline -f repaint
end

function fzf-down
	fzf --height 50% --min-height 20 --border --bind ctrl-p:toggle-preview $argv
end

# https://gist.github.com/aluxian/9c6f97557b7971c32fdff2f2b1da8209
function __git_fzf_is_in_git_repo
	command -s -q git
		and git rev-parse HEAD >/dev/null 2>&1
end

function __git_fzf_git_remote
	__git_fzf_is_in_git_repo; or return
	git remote -v | awk '{print $1 ":" $2}' | uniq | \
		fzf-down -m --ansi --tac \
		--preview 'git log --oneline --graph --date=short --pretty="format:%C(auto)%cd %h%d %s" --remotes={1} | head -200' | \
		cut -d ':' -f1 | \
		fzf_add_to_commandline
end

function __git_fzf_git_status
	__git_fzf_is_in_git_repo; or return
	git -c color.status=always status --short | \
		fzf-down -m --ansi --preview 'git diff --color=always HEAD -- {-1} | head -500' | \
		cut -c4- | \
		sed 's/.* -> //' | \
		fzf_add_to_commandline
end

function __git_fzf_git_branch
	__git_fzf_is_in_git_repo; or return
	git branch -a --color=always | \
		grep -v '/HEAD\s' | \
		fzf-down -m --ansi --preview-window right:70% --preview 'git log --color=always --oneline --graph --date=short \
			--pretty="format:%C(auto)%cd %h%d %s %C(magenta)[%an]%Creset" \
			--print0 \
			--read0 \
			(echo {} | sed s/^..// | cut -d" " -f1) | head -'$LINES | \
		sed 's/^..//' | cut -d' ' -f1 | \
		sed 's#^remotes/##' | \
		fzf_add_to_commandline
end

function __git_fzf_git_tag
	__git_fzf_is_in_git_repo; or return
	git tag --sort -version:refname | \
		fzf-down -m --ansi --preview-window right:70% \
		--preview 'git show --color=always {} | head -'$LINES | \
		fzf_add_to_commandline
end

function __git_fzf_git_log
	__git_fzf_is_in_git_repo; or return
	git log --color=always --graph --date=short --format="%C(auto)%cd %h%d %s %C(magenta)[%an]%Creset" | \
		fzf-down -m --ansi --reverse --preview 'git show --color=always (echo {} | grep -o "[a-f0-9]\{7,\}") | head -'$LINES | \
		sed -E 's/.*([a-f0-9]{7,}).*/\1/' | \
		fzf_add_to_commandline
end

# https://gist.github.com/junegunn/8b572b8d4b5eddd8b85e5f4d40f17236
function git_fzf_key_bindings -d "Set custom key bindings for git+fzf"
	bind \cg\cf __git_fzf_git_status
	bind \cg\cb __git_fzf_git_branch
	bind \cg\ct __git_fzf_git_tag
	bind \cg\ch __git_fzf_git_log
	bind \cg\cr __git_fzf_git_remote
end
