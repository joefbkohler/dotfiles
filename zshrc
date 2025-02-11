export ZSH="$HOME/dotfiles/oh-my-zsh"
ZSH_THEME="ys"

plugins=(git
		 colorize
		 python
		 pip
		 virtualenv
		 ssh
		 sudo)

PYTHON_AUTO_VRUN=true

source $ZSH/oh-my-zsh.sh

alias less=cless
alias cat=ccat

. ~/.localrc &> /dev/null
