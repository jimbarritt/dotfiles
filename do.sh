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
  
  if [ "$DRY_RUN" = true ]; then
    printf "Would execute: ln -sf '%s' '%s'\n" "$target" "$link"
  else
    ln -sf "$target" "$link"
    printf "✓ Created: %s -> %s\n" "$link" "$target"
  fi
}


link_home() {
  local src=$1

  target="${DOTFILES_DIR}/home/${src}"
  link="${HOME}/.${src}"
  create_symlink $target $link 
  
}

link_all() {
  echo "Linking dotfiles into your home dir @ [$HOME]"
}

unlink_all() {
  echo "Putting everything back the way it was"
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
