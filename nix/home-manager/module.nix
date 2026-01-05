# Home-manager module for hypermodern-emacs
#
# Import via flake:
#   inputs.hypermodern-emacs.homeManagerModules.default
#
# Then in home.nix:
#   programs.hypermodern-emacs = {
#     enable = true;
#     server.enable = true;
#   };
#
# Commands:
#   e file.txt  - quick edit (reuse frame)
#   ec          - create new frame
#   et          - terminal mode
#   ed start    - start daemon
#   ed stop     - stop daemon
#   ed log      - view logs
#   ed eval '(+ 1 1)' - eval elisp

{ self, lib }:
{ config, pkgs, ... }:

with lib;

let
  cfg = config.programs.hypermodern-emacs;

  # Get the hypermodern-emacs package builder from our flake lib
  hypermodernEmacs = self.lib."mk-hypermodern-emacs-with-deps" pkgs;

  # Common emacsclient args - DRY up repeated invocations
  clientArgs = "--socket-name=${cfg.server.socketName} --alternate-editor=''";

in {
  options.programs.hypermodern-emacs = {
    enable = mkEnableOption "hypermodern-emacs";
    
    package = mkOption {
      type = types.package;
      default = hypermodernEmacs;
      description = "The hypermodern-emacs package to use.";
    };
    
    server = {
      enable = mkEnableOption "emacs server (daemon mode)";
      
      socketName = mkOption {
        type = types.str;
        default = "server";
        description = "Name of the emacs server socket.";
      };
      
      startWithSession = mkOption {
        type = types.bool;
        default = true;
        description = "Start emacs daemon automatically with graphical session.";
      };
      
      startWithLogin = mkOption {
        type = types.bool;
        default = false;
        description = "Start emacs daemon on login (even without GUI).";
      };
      
      extraArgs = mkOption {
        type = types.listOf types.str;
        default = [];
        description = "Extra arguments to pass to emacs daemon.";
      };
    };
    
    defaultEditor = mkOption {
      type = types.bool;
      default = true;
      description = "Set EDITOR and VISUAL to emacsclient.";
    };
    
    enableBashIntegration = mkOption {
      type = types.bool;
      default = true;
      description = "Enable bash shell integration.";
    };
    
    enableZshIntegration = mkOption {
      type = types.bool;
      default = true;
      description = "Enable zsh shell integration.";
    };
    
    enableFishIntegration = mkOption {
      type = types.bool;
      default = true;
      description = "Enable fish shell integration.";
    };
  };
  
  config = mkIf cfg.enable (mkMerge [
    # Always install the package
    { home.packages = [ cfg.package ]; }
    
    # Server configuration
    (mkIf cfg.server.enable {
      # Systemd user service for emacs daemon
      systemd.user.services.emacs = {
        Unit = {
          Description = "Hypermodern Emacs daemon";
          Documentation = [ "man:emacs(1)" "https://github.com/b7r6/hypermodern-emacs" ];
          After = [ "graphical-session-pre.target" ];
        };
        
        Service = {
          Type = "notify";
          ExecStart = "${cfg.package}/bin/emacs --fg-daemon=${cfg.server.socketName} ${escapeShellArgs cfg.server.extraArgs}";
          ExecStop = "${cfg.package}/bin/emacsclient --socket-name=${cfg.server.socketName} --eval '(kill-emacs)'";
          Restart = "on-failure";
          RestartSec = 3;
          TimeoutStartSec = "300";  # Emacs init can be slow
          
          # Environment
          Environment = [
            "COLORTERM=truecolor"
          ];
          
          # Pass through user's SSH agent
          PassEnvironment = [ "SSH_AUTH_SOCK" "DISPLAY" "WAYLAND_DISPLAY" ];
        };
        
        Install = {
          WantedBy = 
            (optional cfg.server.startWithSession "graphical-session.target") ++
            (optional cfg.server.startWithLogin "default.target");
        };
      };
      
      # Wrapper scripts
      home.packages = with pkgs; [
        # `e` - quick edit (reuse frame or create)
        (writeShellScriptBin "e" ''
          exec ${cfg.package}/bin/emacsclient ${clientArgs} --no-wait "$@"
        '')

        # `ec` - create new frame
        (writeShellScriptBin "ec" ''
          exec ${cfg.package}/bin/emacsclient ${clientArgs} --create-frame "$@"
        '')

        # `et` - terminal mode
        (writeShellScriptBin "et" ''
          exec ${cfg.package}/bin/emacsclient ${clientArgs} --tty "$@"
        '')
        
        # `ed` - daemon control
        (writeShellScriptBin "ed" ''
          case "''${1:-status}" in
            start)
              systemctl --user start emacs
              ;;
            stop)
              systemctl --user stop emacs
              ;;
            restart)
              systemctl --user restart emacs
              ;;
            status)
              systemctl --user status emacs
              ;;
            log|logs)
              journalctl --user -u emacs -f
              ;;
            kill)
              ${cfg.package}/bin/emacsclient ${clientArgs} --eval '(kill-emacs)' 2>/dev/null || true
              ;;
            eval)
              shift
              ${cfg.package}/bin/emacsclient ${clientArgs} --eval "$*"
              ;;
            *)
              echo "Usage: ed {start|stop|restart|status|log|kill|eval EXPR}"
              echo ""
              echo "Commands:"
              echo "  start   - start the daemon"
              echo "  stop    - stop the daemon"
              echo "  restart - restart the daemon"
              echo "  status  - show daemon status"
              echo "  log     - follow daemon logs"
              echo "  kill    - forcefully kill emacs"
              echo "  eval    - evaluate elisp expression"
              exit 1
              ;;
          esac
        '')
      ];
    })
    
    # Editor environment variables
    (mkIf cfg.defaultEditor {
      home.sessionVariables = {
        EDITOR = "emacsclient ${clientArgs} --tty";
        VISUAL = "emacsclient ${clientArgs} --create-frame";
      };
    })

    # Shell integrations - consolidate into one mkIf
    (mkIf cfg.server.enable (mkMerge [
      (mkIf cfg.enableBashIntegration {
        programs.bash.shellAliases.emacs = "emacsclient ${clientArgs} --create-frame";
      })

      (mkIf cfg.enableZshIntegration {
        programs.zsh.shellAliases.emacs = "emacsclient ${clientArgs} --create-frame";
      })

      (mkIf cfg.enableFishIntegration {
        programs.fish.shellAliases.emacs = "emacsclient ${clientArgs} --create-frame";
      })
    ]))
  ]);
}
