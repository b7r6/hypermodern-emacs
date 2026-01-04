{ inputs, ... }:
{
  perSystem = { pkgs, config, ... }:
    let
      inherit (inputs) self;
      emacs = self.lib."mk-hypermodern-emacs" pkgs;
    in
    {
      apps = {
        default = {
          type = "app";
          program = "${config.packages.default}/bin/emacs";
        };

        full = {
          type = "app";
          program = "${config.packages.full}/bin/emacs";
        };

        bare = {
          type = "app";
          program = "${config.packages.bare}/bin/emacs";
        };

        check-env = {
          type = "app";
          program = toString (pkgs.writeShellScript "check-hypermodern-env" ''
            echo "=== hypermodern-emacs environment check ==="
            echo ""
            check() {
              local name=$1 cmd=$2 desc=$3
              if command -v ''${cmd%% *} &>/dev/null && eval "$cmd" &>/dev/null; then
                echo "✓ $name"
              else
                echo "✗ $name - $desc"
              fi
            }
            echo "Optional dependencies:"
            check "lean4" "lean --version" "theorem prover (install via elan)"
            check "cuda/nvcc" "nvcc --version" "NVIDIA CUDA compiler"
            check "tailscale" "tailscale status" "mesh VPN"
            check "gpg" "gpg --version" "encrypted secrets"
            check "nerd-fonts" "fc-list | grep -qi nerd" "icons"
            echo ""
            echo "Language servers (included in .#full):"
            check "basedpyright" "basedpyright --version" "python"
            check "rust-analyzer" "rust-analyzer --version" "rust"
            check "clangd" "clangd --version" "c/c++/cuda"
            check "nixd" "nixd --version" "nix"
            check "typescript-language-server" "typescript-language-server --version" "ts/js"
            check "zls" "zls --version" "zig"
            check "haskell-language-server" "haskell-language-server-wrapper --version" "haskell"
            echo ""
            echo "Use 'nix run .#full' for emacs with LSPs on PATH"
          '');
        };

        test = {
          type = "app";
          program = toString (pkgs.writeShellScript "run-hypermodern-tests" ''
            set -e
            echo "=== hypermodern-emacs test suite ==="
            ${emacs}/bin/emacs --batch \
              -L ${self} -L ${self}/lib -L ${self}/test \
              -l ert \
              -l early-init \
              -l hypermodern-core -l hypermodern-windows -l hypermodern-navigation \
              -l hypermodern-compile -l hypermodern-languages -l hypermodern-terminal \
              -l hypermodern-remote -l hypermodern-secrets \
              -l hypermodern-core-test -l hypermodern-secrets-test \
              -l hypermodern-compile-test \
              -l hypermodern-languages-test -l hypermodern-remote-test \
              -l hypermodern-terminal-test -l hypermodern-integration-test \
              -l hypermodern-build-test -l hypermodern-wiring-test \
              -l hypermodern-windows-test -l hypermodern-navigation-test \
              -f ert-run-tests-batch-and-exit
          '');
        };

        byte-compile = {
          type = "app";
          program = toString (pkgs.writeShellScript "byte-compile-hypermodern" ''
            set -e
            echo "=== Byte-compiling hypermodern-emacs ==="
            workdir=$(mktemp -d)
            trap "rm -rf $workdir" EXIT
            cp ${self}/lib/*.el "$workdir/"
            ${emacs}/bin/emacs --batch \
              -L "$workdir" \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile "$workdir"/*.el
            echo "Byte-compilation successful"
          '');
        };

        lint = {
          type = "app";
          program = toString (pkgs.writeShellScript "lint-hypermodern" ''
            set -e
            echo "=== Linting hypermodern-emacs ==="
            for f in ${self}/lib/*.el; do
              echo "Checking: $f"
              ${emacs}/bin/emacs --batch \
                -L ${self}/lib \
                --eval "(checkdoc-file \"$f\")" 2>&1 | grep -v "^$" || true
            done
            echo "Lint complete"
          '');
        };
      };
    };
}
