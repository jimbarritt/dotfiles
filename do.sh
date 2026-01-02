#!/bin/sh

DRY_RUN="false"
CMD_TO_RUN="link"

DOTFILES_DIR="$(unset CDPATH; cd -- "$(dirname -- "$0")" && pwd)"

slurp_arguments() {
  while [ $# -gt 0 ]; do
    case $1 in
      -d|--dry-run)
        DRY_RUN=true
        shift
        ;;
      install|link|unlink)
	CMD_TO_RUN=$1
	shift
	;;
      -h|--help)
        echo "Usage: $0 [OPTIONS]"
        echo "Options:"
        echo "  -d, --dry-run    Show what would be done without doing it"
        echo "  -h, --help       Show this help message"
        exit 0
        ;;
      *)
        echo "Unknown option: $1"
        echo "Use --help for usage information"
        exit 1
        ;;
    esac
  done
}

create_symlink() {
  target="$1"
  link="$2"
  
  if [ "$DRY_RUN" = "true" ]; then
    printf "[dry-run]: ln -sf '%s' '%s'\n" "$target" "$link"
  else
    ln -sf "$target" "$link"
    printf "✓ Created: %s -> %s\n" "$link" "$target"
  fi
}

remove_symlink() {
  link="$1"
  
  if [ "$DRY_RUN" = "true" ]; then
    printf "[dry-run]: unlink '%s'\n" "$link"
  else
    unlink "$link"
    printf "✓ Removed link: %s\n" "$link"
  fi
}

move_file_to_backup() {
  _link="$1"
  if [ "$DRY_RUN" = "true" ]; then
    printf "[dry-run]: mv '%s' '%s-backed-up'\n" "$_link" "$_link"
  else
    printf "✓ Backed up link: %s\n" "$_link"
  fi
}

# TODO: implement this and an undo!
backup_existing_file() {
  _link="$1"
  if [ -e "$_link" ] && [ ! -L "$_link" ]; then
    echo "A real file exists already @ [$_link]. Backing it up for later"
    move_file_to_backup "$_link"
  fi
}

link_home() {
  _src="$1"

  _target="${DOTFILES_DIR}/home/${_src}"
  _link="${HOME}/.${_src}"

  backup_existing_file "$_link"
  create_symlink "$_target" "$_link"
  
}

unlink_home() {
  _src="$1"

  _link="${HOME}/.${_src}"
  remove_symlink "$_link"
}

link_config() {
  _src="$1"

  _target="${DOTFILES_DIR}/config/${_src}"
  _link="${HOME}/.config/${_src}"

  backup_existing_file "$_link"
  create_symlink "$_target" "$_link"
}

link_all() {
  echo "Linking dotfiles into your home dir @ [$HOME]"
  link_home zshrc
  link_home gitconfig
  link_home tmux.conf

  link_config nvim
  link_config kitty
  link_config mise

  create_symlink "${DOTFILES_DIR}/home/oh-my-zsh/green-tinted.zsh-theme" "${HOME}/.oh-my-zsh/custom/themes/green-tinted.zsh-theme"
}


# TODO: restore backups
unlink_all() {
  echo "Putting everything back the way it was"
  unlink_home zshrc
  unlink_home gitconfig
}

install_zsh_plugins() {
  _plugins_dir="${HOME}/.oh-my-zsh/custom/plugins"
  
  if [ ! -d "${_plugins_dir}/zsh-autopair" ]; then
    echo "Installing zsh-autopair plugin..."
    git clone https://github.com/hlissner/zsh-autopair "${_plugins_dir}/zsh-autopair"
  else
    echo "✓ zsh-autopair already installed"
  fi

  if [ ! -d "${_plugins_dir}/zsh-syntax-highlighting" ]; then
    echo "Installing zsh-syntax-highlighting plugin..."
    git clone https://github.com/zsh-users/zsh-syntax-highlighting.git "${_plugins_dir}/zsh-syntax-highlighting"
  else
    echo "✓ zsh-syntax-highlighting already installed"
  fi
}

install_git_hooks() {
  git config core.hooksPath hooks
  echo "✓ Configured git hooks"
}

install() {
  install_zsh_plugins
  install_git_hooks
}

main() {
  slurp_arguments "$@"

  case $CMD_TO_RUN in
    install)
      install
      ;;
    link)
      link_all
      ;;
    unlink)
      unlink_all
      ;;
    *)
      echo "Invalid command [$CMD_TO_RUN] don't know what to do. Panic!"
      exit 1
      ;;
  esac
  
}

main "$@"
