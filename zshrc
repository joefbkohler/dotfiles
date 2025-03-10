export ZSH="$HOME/dotfiles/oh-my-zsh"
ZSH_THEME="agnoster"

if [ "$TERM" = "linux" ]; then
	ZSH_THEME="ys"
fi

plugins=(colorize
		 python
		 pip
		 virtualenv
		 ssh
		 sudo)

PYTHON_AUTO_VRUN=true
DISABLE_AUTO_UPDATE=true

source $ZSH/oh-my-zsh.sh

HISTFILE=~/.local/state/zshhistfile
HISTSIZE=1000
SAVEHIST=1000
setopt SHARE_HISTORY

alias less=cless
alias cat=ccat
alias su="su -p"

. ~/.localrc &> /dev/null
