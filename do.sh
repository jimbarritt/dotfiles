#!/bin/sh

DRY_RUN=false
COMMAND=link

DOTFILES_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

slurp_arguments() {
  while [[ $# -gt 0 ]]; do
    case $1 in
      -d|--dry-run)
        DRY_RUN=true
        shift
        ;;
      link|unlink)
	COMMAND=$1
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
  local target="$1"
  local link="$2"
  
  if [[ "$DRY_RUN" = true ]]; then
    printf "[dry-run]: ln -sf '%s' '%s'\n" "$target" "$link"
  else
    ln -sf "$target" "$link"
    printf "✓ Created: %s -> %s\n" "$link" "$target"
  fi
}

remove_symlink() {
  local link="$1"
  
  if [[ "$DRY_RUN" = true ]]; then
    printf "[dry-run]: unlink '%s'\n" "$link"
  else
    unlink "$link"
    printf "✓ Removed link: %s\n" "$link"
  fi
}

move_file_to_backup() {
  if [[ "$DRY_RUN" = true ]]; then
    printf "[dry-run]: mv '%s' '%s-backed-up'\n" "$link" "$link"
  else
    printf "✓ Backed up link: %s\n" "$link"
  fi
}

# TODO: implement this and an undo!
backup_existing_file() {
  local link="$1"
  if [[ -e "$link" && ! -L "$link" ]]; then
    echo "A real file exists already @ [$link]. Backing it up for later"
    move_file_to_backup $link
  fi
}

link_home() {
  local src=$1

  target="${DOTFILES_DIR}/home/${src}"
  link="${HOME}/.${src}"

  backup_existing_file $link
  create_symlink $target $link 
  
}

unlink_home() {
  local src=$1

  link="${HOME}/.${src}"
  remove_symlink $link
}

link_config() {
  local src=$1

  target="${DOTFILES_DIR}/config/${src}"
  link="${HOME}/.config/${src}"

  backup_existing_file $link
  create_symlink $target $link
}

link_all() {
  echo "Linking dotfiles into your home dir @ [$HOME]"
  link_home zshrc
  link_home gitconfig
  link_config nvim
  link_config kitty
}


# TODO: restore backups
unlink_all() {
  echo "Putting everything back the way it was"
  unlink_home zshrc
  unlink_home gitconfig
}

main() {
  slurp_arguments "$@"

  case $COMMAND in
    link)
      link_all
      ;;
    unlink)
      unlink_all
      ;;
    *)
      echo "Invalid command [$command] don't know what to do. Panic!"
      exit 1
      ;;
  esac
  
}

main "$@"
