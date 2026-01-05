## `// hypermodern // emacs`
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

this is an `emacs` configuration. whether it is your `emacs` configuration is, in accordance with our
agenda, your decision.

## // your config: `user-config.el`

**don't know where to put your settings? → `user-config.el`**

that's your file. appearance, keybindings, package tweaks, personal functions — all goes there.
it's well-commented with examples. `init.el` loads it at the end so your settings override everything.

```
all the speed he took, all the turns he'd taken and the corners he'd cut
in night city, and still he'd see the matrix in his sleep, bright lattices
of logic unfolding across that colorless void...
```

## // architecture

```
...burgeoning technologies require outlaw zones, that Night City wasn't
there for its inhabitants, but as a deliberately unsupervised playground for
technology itself.
```

```
init.el                     # the configuration
early-init.el               # fast startup bootstrap
lib/                        # hypermodern-* libraries
  hypermodern-core.el       # quick wins, project.el, wgrep
  hypermodern-ui.el         # unified visual control
  hypermodern-languages.el  # eglot, treesit, languages
  hypermodern-terminal.el   # vterm, eat, detached
  hypermodern-remote.el     # tramp, tailscale, secrets
  hypermodern-ai.el         # llm integration
themes/                     # chromatic decisions
  ono-sendai.nix            # dark series palettes
  maas.nix                  # light series palettes
  *.svg                     # agency sheets
flake.nix                   # nix packaging
install.sh                  # non-nix bootstrap
```

## // usage

### with nix

```bash
# run directly (uses flake's init.el)
nix run github:b7r6/hypermodern-emacs

# with language servers on PATH
nix run github:b7r6/hypermodern-emacs#full

# bare emacs (uses ~/.emacs.d, for custom configs)
nix run github:b7r6/hypermodern-emacs#bare

# add to flake inputs
inputs.hypermodern-emacs.url = "github:b7r6/hypermodern-emacs";

# home-manager module
programs.hypermodern-emacs.enable = true;
```

outputs:
- `default` / `full` — configured emacs (init.el baked in via `--init-directory`)
- `bare` / `bare-full` — packages only (uses `~/.emacs.d`)
- `config` — just the config directory
- `libs` — just the lib/*.el files

### without nix

```bash
git clone https://github.com/b7r6/hypermodern-emacs ~/.config/hypermodern-emacs
cd ~/.config/hypermodern-emacs
./install.sh
```

first launch downloads packages from MELPA. this takes 2-3 minutes.

## // philosophy

```
mirrors, someone has once said, were in some way essentially unwholesome.
constructs were more so, she decided.
```

- **`deterministic`**: works the same across machines and timelines
- **`minimal`**: no engagement theater, no visual noise
- **`composable`**: unix philosophy for the post-unix era
- **`aesthetic`**: form follows function follows form

## // chromatic systems

### ono-sendai (dark)

| variant | L% | character |
|---------|-----|-----------|
| memphis | 0% | true black, OLED perfect, kills some fonts |
| chiba | 4% | deep void, problematic on samsung panels |
| razorgirl | 8% | preserves font strokes |
| **tuned** | **11%** | **daily driver, perceptual uniformity** |
| github | 16% | maximum compatibility |

### maas biolabs (light)

| variant | L% | character |
|---------|-----|-----------|
| **neoform** | **97%** | **daily driver, blue-tinted paper** |
| bioptic | 97% | warm cream, long reading |
| ghost | 92% | low contrast, photosensitivity |
| tessier | 100% | clinical, maximum contrast |

all themes maintain 211° hue discipline. the hero blue shifts darker on light backgrounds
to preserve visual weight.

## // keybindings

| key | action |
|-----|--------|
| `C-c o u` | UI control panel |
| `C-c p` | project prefix (find-file, ripgrep, etc) |
| `C-c s` | secrets menu |
| `C-c g g` | gptel chat |
| `C-c l r` | rename (eglot) |
| `C-c l a` | code actions |
| `M-.` / `M-,` / `M-?` | xref: definition / back / references |
| `M-z` | format buffer |
| `C-\`` | toggle terminal |
| `C-c t f` | tailscale find file |
| `C-c t s` | tailscale shell |
| `C-c d c` | detached command |
| `C-c d l` | list detached sessions |
| `C-x g` | magit |

## // secrets

multi-context agenix-native secrets. define contexts in `hypermodern/secrets-contexts`:

```elisp
'((personal
   :name "Personal"
   :authinfo "~/.authinfo.gpg"
   :agenix-secrets "/run/agenix"
   :agenix-source "~/.config/agenix")
  (weyl
   :name "Weyl AI"
   :authinfo "~/weyl/.authinfo.gpg"
   :agenix-secrets "/run/agenix"
   :agenix-source "~/weyl/secrets"))
```

`C-c s` opens the secrets transient menu. context auto-detects from hostname or `HYPERMODERN_CONTEXT` env.

agenix secrets are readable via `auth-source-search` — gptel just works if you have `/run/agenix/openrouter-api-key`.

## // language servers

eglot with the protocol wars resolved. inlay hints off by default.

| language | server |
|----------|--------|
| python | basedpyright (or `M-x hypermodern/python-use-ruff`) |
| typescript/js | typescript-language-server |
| rust | rust-analyzer |
| c/c++/cuda | clangd (--query-driver for nvcc) |
| haskell | haskell-language-server |
| nix | nixd |
| zig | zls |
| bash | bash-language-server |
| lean4 | lake serve (via lsp-mode, `C-c C-i` for infoview) |

bazel first: `M-x hypermodern/bazel-compile-commands` generates compile_commands.json via hedron.

## // remote

tailscale integration with magic DNS:

```
C-c t f    find file on tailscale host
C-c t s    shell on tailscale host
C-c t d    dired on tailscale host
```

tramp just works with ControlMaster. edit bashrc on any host: `M-x hypermodern/remote-bashrc`

detached.el is the tmux answer — commands persist after emacs exits:

```
C-c d c    run command detached
C-c d o    attach to session
C-c d l    list sessions
```

## // requirements

- `emacs` 29+ (30 recommended for tree-sitter)
- desultory acknowledgment of late capitalism's technical victories
- a sense that there is still a way to live well in this digital world

## // testing

this is real software with real tests.

```bash
# run the full ERT test suite
nix run .#test

# byte-compile (catches undefined references)
nix run .#byte-compile

# lint with checkdoc
nix run .#lint

# run all checks (nix flake check)
nix flake check

# check your environment
nix run .#check-env
```

CI runs weekly to catch upstream breakage. tests cover:

- module loading and initialization
- secrets context switching (state machine invariants)
- auth-source integration
- eglot server configuration
- keybinding conflicts
- startup performance

add tests when you add features. `test/` has the patterns.

## // recommended

- `berkeley mono` and related lifelong investments
- displays with proper black level handling
- `nix` for reproducibility across timelines

## // notes

```
he'd always imagined it as a gradual and willing accommodation of
the machine, the parent organism. it was the root of street cool too,
the knowing posture that implied connection, invisible lines up to
hidden levels of influence.
```

no warranty is implied: it is outright stated. if you wish to use this code,
we will help you to the greatest possible extent.

━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
