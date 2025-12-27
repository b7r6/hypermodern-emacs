{
  description = "hypermodern emacs — cyberpunk editor for the discerning operator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      # Theme definitions (importable separately)
      onoSendaiThemes = import ./themes/ono-sendai.nix;
      maasThemes = import ./themes/maas.nix;
      allThemes = onoSendaiThemes // maasThemes;

      # Helper to build a base16 theme package from a scheme
      mkBase16Theme = pkgs: epkgs: scheme:
        let
          themeSym = "base16-${scheme.slug}";
          colorsVar = "${themeSym}-theme-colors";
          p = scheme.palette;
          c = k: builtins.getAttr k p;
        in
        epkgs.trivialBuild {
          pname = "${themeSym}-theme";
          version = "0.1.0";
          src = pkgs.runCommand "${themeSym}-theme-source" { } ''
            mkdir -p $out
            cat > $out/${themeSym}-theme.el << 'EOF'
;;; ${themeSym}-theme.el --- Base16 ${scheme.name} theme -*- lexical-binding: t; -*-
(require 'base16-theme)

(defvar ${colorsVar}
  '(:base00 "${c "base00"}"
    :base01 "${c "base01"}"
    :base02 "${c "base02"}"
    :base03 "${c "base03"}"
    :base04 "${c "base04"}"
    :base05 "${c "base05"}"
    :base06 "${c "base06"}"
    :base07 "${c "base07"}"
    :base08 "${c "base08"}"
    :base09 "${c "base09"}"
    :base0A "${c "base0A"}"
    :base0B "${c "base0B"}"
    :base0C "${c "base0C"}"
    :base0D "${c "base0D"}"
    :base0E "${c "base0E"}"
    :base0F "${c "base0F"}")
  "Base16 palette for ${themeSym}.")

(deftheme ${themeSym})
(base16-theme-define '${themeSym} ${colorsVar})
(provide-theme '${themeSym})
(provide '${themeSym}-theme)
;;; ${themeSym}-theme.el ends here
EOF
          '';
          packageRequires = [ epkgs.base16-theme ];
        };

      # Build the local libs as a package
      mkLocalLibs = pkgs: pkgs.symlinkJoin {
        name = "hypermodern-emacs-libs";
        paths = builtins.attrValues (
          builtins.mapAttrs
            (name: _:
              pkgs.writeTextFile {
                inherit name;
                text = builtins.readFile (./lib + "/${name}");
                destination = "/share/emacs/site-lisp/${name}";
              })
            (nixpkgs.lib.filterAttrs
              (n: _: nixpkgs.lib.hasSuffix ".el" n)
              (builtins.readDir ./lib))
        );
      };

      # The core emacs packages list
      corePackages = pkgs: epkgs:
        let
          # lean4-mode isn't in MELPA, build from source
          # To get sha256: nix-prefetch-github leanprover-community lean4-mode
          lean4-mode = epkgs.melpaBuild {
            pname = "lean4-mode";
            version = "1.0.0";
            src = pkgs.fetchFromGitHub {
              owner = "leanprover-community";
              repo = "lean4-mode";
              rev = "master";
              sha256 = "sha256-6XFcyqSTx1CwNWqQvIc25cuQMwh3YXnbgr5cDiOCxBk=";
            };
            packageRequires = with epkgs; [ dash f flycheck magit-section lsp-mode s ];
            recipe = pkgs.writeText "recipe" ''
              (lean4-mode :repo "leanprover-community/lean4-mode" :fetcher github :files ("*.el" "data"))
            '';
          };
        in
        with epkgs; [
        base16-theme
        doom-modeline
        nerd-icons
        dashboard

        # UI / minibuffer
        vertico
        orderless
        marginalia
        consult
        embark
        embark-consult
        general
        which-key
        popper
        shackle

        # optional UI candy
        dimmer
        solaire-mode
        ligature
        mixed-pitch
        olivetti
        nerd-icons-completion
        rainbow-mode
        transient

        # icons and completion
        all-the-icons
        all-the-icons-completion
        company
        yasnippet

        # programming modes
        typescript-mode
        js2-mode
        web-mode
        yaml-mode
        markdown-mode
        json-mode
        dockerfile-mode
        csharp-mode
        fsharp-mode
        haskell-mode
        nix-mode
        cuda-mode
        zig-mode
        nasm-mode
        bazel
        lean4-mode

        # lsp-mode needed for lean4-mode
        lsp-mode
        lsp-ui

        # lisp editing
        paredit
        paredit-everywhere
        smartparens

        # fuzzy finding
        fzf

        # common packages
        exec-path-from-shell
        gcmh
        undo-fu
        undo-fu-session
        expand-region
        multiple-cursors
        ace-window
        avy
        helpful
        wgrep

        # quality of life
        rg
        direnv
        vterm
        eat
        detached
        clipetty
        pass

        # git
        magit
        forge

        # AI
        gptel

        # LSP
        lsp-mode
        lsp-ui
        lsp-pyright
        lsp-haskell
        treesit-auto
        format-all

        # optional apps
        atomic-chrome
        elfeed
        pdf-tools
        nov
      ];

      # Build wrapped emacs with all packages
      mkHypermodernEmacs = pkgs:
        let
          localLibs = mkLocalLibs pkgs;
          themeSchemes = nixpkgs.lib.filterAttrs
            (_: v: (v ? palette) && (v ? slug) && (v ? name))
            allThemes;
        in
        (pkgs.emacsPackagesFor pkgs.emacs30-pgtk).emacsWithPackages (epkgs:
          let
            themePkgs = nixpkgs.lib.mapAttrsToList
              (_: scheme: mkBase16Theme pkgs epkgs scheme)
              themeSchemes;
          in
          (corePackages pkgs epkgs) ++ themePkgs
        );

      # Runtime dependencies (language servers, formatters, tools)
      # Override per-system or per-user as needed
      runtimeDeps = pkgs: with pkgs; [
        # Language servers
        basedpyright              # python
        nodePackages.typescript-language-server  # typescript/js
        rust-analyzer             # rust
        clang-tools               # clangd for c/c++/cuda
        haskell-language-server   # haskell
        nixd                      # nix
        zls                       # zig
        nodePackages.bash-language-server  # bash
        marksman                  # markdown
        nodePackages.vscode-json-languageserver  # json
        yaml-language-server      # yaml

        # Formatters
        nixfmt-rfc-style          # nix
        nodePackages.prettier     # js/ts/css/html
        rustfmt                   # rust (usually comes with rust-analyzer)
        ormolu                    # haskell

        # Build tools
        bazel                     # bazel
        gnumake
        cmake                     # for when you must

        # Search/navigation
        ripgrep
        fd
        fzf

        # Git
        git
        delta                     # better diffs

        # Misc
        direnv
        nix-direnv
      ];

      # Emacs wrapped with runtime deps on PATH
      mkHypermodernEmacsWithDeps = pkgs:
        let
          emacs = mkHypermodernEmacs pkgs;
          deps = runtimeDeps pkgs;
        in
        pkgs.symlinkJoin {
          name = "hypermodern-emacs-full";
          paths = [ emacs ];
          nativeBuildInputs = [ pkgs.makeWrapper ];
          postBuild = ''
            wrapProgram $out/bin/emacs \
              --prefix PATH : ${pkgs.lib.makeBinPath deps}
            wrapProgram $out/bin/emacsclient \
              --prefix PATH : ${pkgs.lib.makeBinPath deps}
          '';
          passthru = {
            inherit deps;
            unwrapped = emacs;
            
            # What this flake expects from YOUR environment
            # (not packaged here, you bring these)
            envDeps = {
              # Lean 4 (install via elan)
              lean = {
                description = "Lean 4 theorem prover";
                install = "curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh";
                check = "lean --version";
                required = false;
              };
              
              # CUDA (system-level)
              cuda = {
                description = "NVIDIA CUDA toolkit";
                install = "system package manager or runfile from nvidia.com";
                check = "nvcc --version";
                required = false;
                notes = "clangd needs --query-driver for nvcc, already configured";
              };
              
              # Tailscale (for remote features)
              tailscale = {
                description = "Tailscale mesh VPN";
                install = "https://tailscale.com/download";
                check = "tailscale status";
                required = false;
              };
              
              # GPG (for secrets)
              gpg = {
                description = "GnuPG for encrypted authinfo";
                install = "system package manager";
                check = "gpg --version";
                required = false;
                notes = "needed for .authinfo.gpg, agenix";
              };
              
              # Agenix (for secrets)
              agenix = {
                description = "age-encrypted secrets for NixOS";
                install = "nix profile install github:ryantm/agenix";
                check = "agenix --help";
                required = false;
              };
              
              # Fonts (for icons)
              fonts = {
                description = "Nerd Fonts for icons";
                install = "nix profile install nixpkgs#nerdfonts";
                check = "fc-list | grep -i nerd";
                required = false;
                notes = "doom-modeline and dashboard use nerd-icons";
              };
            };
          };
        };

    in
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        localLibs = mkLocalLibs pkgs;
        
        # Bundle the config files into a directory
        configDir = pkgs.runCommand "hypermodern-emacs-config" {} ''
          mkdir -p $out/lib $out/themes
          cp ${./init.el} $out/init.el
          cp ${./early-init.el} $out/early-init.el
          for f in ${./lib}/*.el; do
            cp "$f" $out/lib/
          done
          for f in ${./themes}/*.el; do
            cp "$f" $out/themes/
          done
        '';
        
        # Wrapper that uses our config via --init-directory (emacs 29+)
        wrapWithConfig = emacsPkg: let
          wrapped = pkgs.runCommand "hypermodern-emacs-configured" {
            nativeBuildInputs = [ pkgs.makeWrapper ];
            meta.mainProgram = "emacs";
          } ''
            mkdir -p $out/bin
            
            # Create wrapper for emacs
            makeWrapper ${emacsPkg}/bin/emacs $out/bin/emacs \
              --add-flags "--init-directory=${configDir}"
            
            # Link emacsclient directly (doesn't need config)
            ln -s ${emacsPkg}/bin/emacsclient $out/bin/emacsclient
            
            # Also provide unwrapped versions
            ln -s ${emacsPkg}/bin/emacs $out/bin/emacs-unwrapped
          '';
        in wrapped // {
          passthru = {
            unwrapped = emacsPkg;
            inherit configDir;
          };
        };
      in
      {
        packages = {
          # Configured: uses flake's init.el
          default = wrapWithConfig (mkHypermodernEmacs pkgs);
          
          # Configured with LSPs on PATH  
          full = wrapWithConfig (mkHypermodernEmacsWithDeps pkgs);
          
          # Bare: uses ~/.emacs.d (for installing elsewhere)
          bare = mkHypermodernEmacs pkgs;
          bare-full = mkHypermodernEmacsWithDeps pkgs;
          
          # Components
          libs = localLibs;
          config = configDir;
        };

        # nix run .# — configured emacs
        apps.default = {
          type = "app";
          program = "${wrapWithConfig (mkHypermodernEmacs pkgs)}/bin/emacs";
        };
        
        # nix run .#full — with LSPs
        apps.full = {
          type = "app";
          program = "${wrapWithConfig (mkHypermodernEmacsWithDeps pkgs)}/bin/emacs";
        };
        
        # nix run .#bare — uses ~/.emacs.d
        apps.bare = {
          type = "app";
          program = "${mkHypermodernEmacs pkgs}/bin/emacs";
        };
        
        # Check what env deps are missing
        apps.check-env = {
          type = "app";
          program = toString (pkgs.writeShellScript "check-hypermodern-env" ''
            echo "=== hypermodern-emacs environment check ==="
            echo ""
            
            check() {
              name=$1
              cmd=$2
              desc=$3
              if command -v ''${cmd%% *} &>/dev/null && eval "$cmd" &>/dev/null; then
                echo "✓ $name"
              else
                echo "✗ $name - $desc"
                echo "  check: $cmd"
              fi
            }
            
            echo "Optional dependencies:"
            check "lean4" "lean --version" "theorem prover (install via elan)"
            check "elan" "elan --version" "lean toolchain manager"
            check "cuda/nvcc" "nvcc --version" "NVIDIA CUDA compiler"
            check "tailscale" "tailscale status" "mesh VPN for remote editing"
            check "gpg" "gpg --version" "for encrypted secrets"
            check "agenix" "agenix --help" "age-encrypted NixOS secrets"
            check "nerd-fonts" "fc-list | grep -qi nerd" "icons in modeline/dashboard"
            
            echo ""
            echo "Language servers (included in .#full):"
            check "basedpyright" "basedpyright --version" "python LSP"
            check "rust-analyzer" "rust-analyzer --version" "rust LSP"
            check "clangd" "clangd --version" "c/c++/cuda LSP"
            check "nixd" "nixd --version" "nix LSP"
            check "typescript-language-server" "typescript-language-server --version" "ts/js LSP"
            check "zls" "zls --version" "zig LSP"
            check "haskell-language-server" "haskell-language-server-wrapper --version" "haskell LSP"
            
            echo ""
            echo "Use 'nix run .#full' for emacs with LSPs on PATH"
          '');
        };
        
        # Run ERT test suite
        apps.test = {
          type = "app";
          program = toString (pkgs.writeShellScript "run-hypermodern-tests" ''
            set -e
            echo "=== hypermodern-emacs test suite ==="
            ${mkHypermodernEmacs pkgs}/bin/emacs --batch \
              -L ${./lib} \
              -L ${./test} \
              -l ert \
              -l hypermodern-core \
              -l hypermodern-languages \
              -l hypermodern-terminal \
              -l hypermodern-remote \
              -l hypermodern-secrets \
              -l hypermodern-core-test \
              -l hypermodern-secrets-test \
              -l hypermodern-languages-test \
              -l hypermodern-remote-test \
              -l hypermodern-terminal-test \
              -l hypermodern-integration-test \
              -f ert-run-tests-batch-and-exit
          '');
        };
        
        # Byte-compile all elisp (catches undefined refs)
        apps.byte-compile = {
          type = "app";
          program = toString (pkgs.writeShellScript "byte-compile-hypermodern" ''
            set -e
            echo "=== Byte-compiling hypermodern-emacs ==="
            ${mkHypermodernEmacs pkgs}/bin/emacs --batch \
              -L ${./lib} \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile \
              ${./lib}/*.el
            echo "Byte-compilation successful"
          '');
        };
        
        # Lint with checkdoc
        apps.lint = {
          type = "app";
          program = toString (pkgs.writeShellScript "lint-hypermodern" ''
            set -e
            echo "=== Linting hypermodern-emacs ==="
            for f in ${./lib}/*.el; do
              echo "Checking: $f"
              ${mkHypermodernEmacs pkgs}/bin/emacs --batch \
                -L ${./lib} \
                --eval "(checkdoc-file \"$f\")" 2>&1 | grep -v "^$" || true
            done
            echo "Lint complete"
          '');
        };

        # Nix flake checks (run by `nix flake check`)
        checks = {
          # Quick smoke test - does it build and start?
          build = mkHypermodernEmacs pkgs;
          
          # Full build with deps
          build-full = mkHypermodernEmacsWithDeps pkgs;
          
          # Test suite
          test = pkgs.runCommand "hypermodern-emacs-test" {
            nativeBuildInputs = [ (mkHypermodernEmacs pkgs) ];
          } ''
            export HOME=$(mktemp -d)
            emacs --batch \
              -L ${./lib} \
              -L ${./test} \
              -l ert \
              -l hypermodern-core \
              -l hypermodern-languages \
              -l hypermodern-terminal \
              -l hypermodern-remote \
              -l hypermodern-secrets \
              -l hypermodern-core-test \
              -l hypermodern-secrets-test \
              -l hypermodern-languages-test \
              -l hypermodern-remote-test \
              -l hypermodern-terminal-test \
              -f ert-run-tests-batch-and-exit
            touch $out
          '';
          
          # Byte-compile check
          byte-compile = pkgs.runCommand "hypermodern-emacs-byte-compile" {
            nativeBuildInputs = [ (mkHypermodernEmacs pkgs) ];
          } ''
            export HOME=$(mktemp -d)
            emacs --batch \
              -L ${./lib} \
              --eval "(setq byte-compile-error-on-warn t)" \
              -f batch-byte-compile \
              ${./lib}/*.el
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          packages = [
            (mkHypermodernEmacsWithDeps pkgs)
          ] ++ (runtimeDeps pkgs);
        };
      }
    ) // {
      # Home-manager module
      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.hypermodern-emacs;
          localLibs = mkLocalLibs pkgs;
          themeSchemes = lib.filterAttrs
            (_: v: (v ? palette) && (v ? slug) && (v ? name))
            allThemes;
        in
        {
          options.programs.hypermodern-emacs = {
            enable = lib.mkEnableOption "hypermodern emacs configuration";

            package = lib.mkOption {
              type = lib.types.package;
              default = pkgs.emacs30-pgtk;
              description = "The Emacs package to use";
            };

            extraPackages = lib.mkOption {
              type = lib.types.functionTo (lib.types.listOf lib.types.package);
              default = _: [ ];
              description = "Extra Emacs packages to install";
            };

            stylixTheme = lib.mkOption {
              type = lib.types.nullOr lib.types.attrs;
              default = null;
              description = "Stylix base16 scheme to generate theme from";
            };
          };

          config = lib.mkIf cfg.enable {
            # Disable stylix emacs target if using this module
            stylix.targets.emacs.enable = lib.mkDefault false;

            xdg.configFile."emacs/early-init.el".source = ./early-init.el;
            home.file.".emacs.d/early-init.el".source = ./early-init.el;

            programs.emacs = {
              enable = true;
              package = (pkgs.emacsPackagesFor cfg.package).emacsWithPackages (epkgs:
                let
                  themePkgs = lib.mapAttrsToList
                    (_: scheme: mkBase16Theme pkgs epkgs scheme)
                    themeSchemes;

                  stylixPkg = lib.optional (cfg.stylixTheme != null) (
                    let s = cfg.stylixTheme; in
                    epkgs.trivialBuild {
                      pname = "base16-stylix-theme";
                      version = "1.0.0";
                      src = pkgs.runCommand "base16-stylix-theme-source" { } ''
                        mkdir -p $out
                        cat > $out/base16-stylix-theme.el << 'EOF'
;;; base16-stylix-theme.el --- Stylix theme -*- lexical-binding: t; -*-
(require 'base16-theme)
(defvar base16-stylix-theme-colors
  '(:base00 "${s.base00}" :base01 "${s.base01}" :base02 "${s.base02}" :base03 "${s.base03}"
    :base04 "${s.base04}" :base05 "${s.base05}" :base06 "${s.base06}" :base07 "${s.base07}"
    :base08 "${s.base08}" :base09 "${s.base09}" :base0A "${s.base0A}" :base0B "${s.base0B}"
    :base0C "${s.base0C}" :base0D "${s.base0D}" :base0E "${s.base0E}" :base0F "${s.base0F}"))
(deftheme base16-stylix)
(base16-theme-define 'base16-stylix base16-stylix-theme-colors)
(provide-theme 'base16-stylix)
(provide 'base16-stylix-theme)
EOF
                      '';
                      packageRequires = [ epkgs.base16-theme ];
                    }
                  );
                in
                (corePackages pkgs epkgs) ++ themePkgs ++ stylixPkg ++ (cfg.extraPackages epkgs)
              );

              extraConfig = ''
                ;; Load hypermodern libs
                (add-to-list 'load-path "${localLibs}/share/emacs/site-lisp")

                ;; Load ono-sendai themes
                ${lib.concatMapStringsSep "\n" (scheme: ''
                  (ignore-errors
                    (require 'base16-${scheme.slug}-theme)
                    (add-to-list 'custom-theme-load-path
                      (file-name-directory (locate-library "base16-${scheme.slug}-theme"))))
                '') (lib.attrValues themeSchemes)}

                ;; Main config
                ${builtins.readFile ./init.el}
              '';
            };

            home.packages = with pkgs; [
              localLibs
              ripgrep
              fd
              gnupg
              pass
            ];
          };
        };

      # Export themes for other configs
      lib.themes = allThemes;
    };
}
