# Update this if you have forked rice-config
rice-config := "github:emacs-twist/rice-config"

# Specify a flake reference to a repository and branch where the package recipe
# is defined.
melpa := "github:akirak/melpa/akirak"

# Relative path to the lock directory
lock-dir := ".rice-lock/default"

# This is only to avoid repetition, and you usually don't edit this.
common-options := "--override-input rice-src \"path:$PWD\" --override-input melpa " + quote(melpa)

# The name of an Emacs package from nix-emacs-ci
emacs := "emacs-release-snapshot"

# Name of the package under test
package := "twind"

# Don't edit this
arch := shell('nix eval --expr builtins.currentSystem --impure --raw')

# Show the flake
show *OPTIONS:
    nix flake show {{ rice-config }} {{ OPTIONS }} {{ common-options }} --override-input systems github:nix-systems/{{ arch }} --allow-import-from-derivation

# Evaluate an attribute on the flake, e.g. just eval melpaRecipes.
eval ATTR *OPTIONS:
    nix eval {{rice-config}}\#{{ATTR}} {{OPTIONS}} {{ common-options }}

# Generate a lock directory.
lock *OPTIONS:
    nix run github:emacs-twist/rice-init-lock\#lock-with-{{ emacs }} --impure -- {{ OPTIONS }} {{ lock-dir }}

# Enter a shell for byte-compiling individual source files
shell-compile:
    nix develop {{ rice-config }}\#{{ emacs }}-for-{{ package }} {{ common-options }}

# Re-run byte-compile every time a file is modified
watch-compile:
    nix develop {{ rice-config }}\#{{ emacs }}-for-{{ package }} {{ common-options }} -c bash -c 'echo >&2 Watching *.el; ls *.el | entr -p elisp-byte-compile /_'

# Byte-compile the package
check-compile:
    nix build {{ rice-config }}\#checks.{{ arch }}.{{ package }}-compile-{{ emacs }} {{ common-options }} --print-build-logs

# Enter a shell for running tests
shell-emacs *OPTIONS:
    nix shell {{ rice-config }}\#{{ emacs }}-with-packages {{ common-options }} {{ OPTIONS }}
