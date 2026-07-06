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
      install|link|unlink|link-claude|unlink-claude)
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
    ln -shf "$target" "$link"
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

ensure_dir() {
  _dir="$1"
  if [ "$DRY_RUN" = "true" ]; then
    printf "[dry-run]: mkdir -p '%s'\n" "$_dir"
  else
    mkdir -p "$_dir"
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

link_claude() {
  echo "Linking Claude config into ~/.claude/"

  ensure_dir "${HOME}/.claude/skills"
  ensure_dir "${HOME}/.claude/hooks"
  ensure_dir "${HOME}/.claude/themes"

  backup_existing_file "${HOME}/.claude/CLAUDE.md"
  create_symlink "${DOTFILES_DIR}/home/claude/CLAUDE.md" "${HOME}/.claude/CLAUDE.md"

  backup_existing_file "${HOME}/.claude/settings.json"
  create_symlink "${DOTFILES_DIR}/home/claude/settings.json" "${HOME}/.claude/settings.json"

  backup_existing_file "${HOME}/.claude/keybindings.json"
  create_symlink "${DOTFILES_DIR}/home/claude/keybindings.json" "${HOME}/.claude/keybindings.json"

  backup_existing_file "${HOME}/.claude/statusline-command.sh"
  create_symlink "${DOTFILES_DIR}/home/claude/statusline-command.sh" "${HOME}/.claude/statusline-command.sh"

  for _skill_dir in "${DOTFILES_DIR}/home/claude/skills"/*/; do
    _skill_name=$(basename "${_skill_dir%/}")
    create_symlink "${_skill_dir%/}" "${HOME}/.claude/skills/${_skill_name}"
  done

  for _hook in "${DOTFILES_DIR}/home/claude/hooks/"*.sh; do
    _hook_name=$(basename "$_hook")
    create_symlink "$_hook" "${HOME}/.claude/hooks/${_hook_name}"
  done

  for _theme in "${DOTFILES_DIR}/home/claude/themes/"*; do
    _theme_name=$(basename "$_theme")
    create_symlink "$_theme" "${HOME}/.claude/themes/${_theme_name}"
  done
}

unlink_claude() {
  echo "Removing Claude config symlinks from ~/.claude/"

  remove_symlink "${HOME}/.claude/CLAUDE.md"
  remove_symlink "${HOME}/.claude/settings.json"
  remove_symlink "${HOME}/.claude/keybindings.json"
  remove_symlink "${HOME}/.claude/statusline-command.sh"

  for _skill_dir in "${DOTFILES_DIR}/home/claude/skills"/*/; do
    _skill_name=$(basename "${_skill_dir%/}")
    remove_symlink "${HOME}/.claude/skills/${_skill_name}"
  done

  for _hook in "${DOTFILES_DIR}/home/claude/hooks/"*.sh; do
    _hook_name=$(basename "$_hook")
    remove_symlink "${HOME}/.claude/hooks/${_hook_name}"
  done

  for _theme in "${DOTFILES_DIR}/home/claude/themes/"*; do
    _theme_name=$(basename "$_theme")
    remove_symlink "${HOME}/.claude/themes/${_theme_name}"
  done
}

link_copilot() {
  echo "Linking global CLAUDE.md and skills into ~/.copilot/"

  ensure_dir "${HOME}/.copilot/skills"

  backup_existing_file "${HOME}/.copilot/copilot-instructions.md"
  create_symlink "${DOTFILES_DIR}/home/claude/CLAUDE.md" "${HOME}/.copilot/copilot-instructions.md"

  backup_existing_file "${HOME}/.copilot/allowed-commands"
  create_symlink "${DOTFILES_DIR}/home/copilot/allowed-commands" "${HOME}/.copilot/allowed-commands"

  for _skill_dir in "${DOTFILES_DIR}/home/claude/skills"/*/; do
    _skill_name=$(basename "${_skill_dir%/}")
    create_symlink "${_skill_dir%/}" "${HOME}/.copilot/skills/${_skill_name}"
  done
}

unlink_copilot() {
  echo "Removing Copilot symlinks from ~/.copilot/"

  remove_symlink "${HOME}/.copilot/copilot-instructions.md"
  remove_symlink "${HOME}/.copilot/allowed-commands"

  for _skill_dir in "${DOTFILES_DIR}/home/claude/skills"/*/; do
    _skill_name=$(basename "${_skill_dir%/}")
    remove_symlink "${HOME}/.copilot/skills/${_skill_name}"
  done
}

link_all() {
  echo "Linking dotfiles into your home dir @ [$HOME]"
  link_home zshrc
  link_home gitconfig
  link_home tmux.conf
  link_home hammerspoon

  link_config nvim
  link_config kitty
  link_config ghostty
  link_config flashspace
  link_config mise

  create_symlink "${DOTFILES_DIR}/home/oh-my-zsh/green-tinted.zsh-theme" "${HOME}/.oh-my-zsh/custom/themes/green-tinted.zsh-theme"
  create_symlink "${DOTFILES_DIR}/bin" "${HOME}/bin"
  link_config key-help
  link_claude
  link_copilot
}


# TODO: restore backups
unlink_all() {
  echo "Putting everything back the way it was"
  unlink_home zshrc
  unlink_home gitconfig
  unlink_home hammerspoon
  unlink_claude
  unlink_copilot
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

configure_macos() {
  echo "Applying macOS defaults..."
  # Disable window shadow on Ghostty — shadows bleed onto adjacent tiled windows
  defaults write com.mitchellh.ghostty NSWindowShadowEnabled -bool false
  echo "✓ Ghostty: window shadow disabled"
}

install() {
  install_zsh_plugins
  install_git_hooks
  configure_macos
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
    link-claude)
      link_claude
      ;;
    link-copilot)
      link_copilot
      ;;
    unlink)
      unlink_all
      ;;
    unlink-claude)
      unlink_claude
      ;;
    unlink-copilot)
      unlink_copilot
      ;;
    *)
      echo "Invalid command [$CMD_TO_RUN] don't know what to do. Panic!"
      exit 1
      ;;
  esac
  
}

main "$@"
