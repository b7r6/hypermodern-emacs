;;; base16-maas-neoform-theme.el --- Maas Neoform -*- lexical-binding: t; -*-
;;
;; Author: b7r6
;; URL: https://github.com/b7r6/hypermodern-emacs
;;
;; Clean room schematics — daily driver light mode

(require 'base16-theme)

(defvar base16-maas-neoform-theme-colors
  '(:base00 "#f5f7fa"  ; HSL(211° 28% 97%) - paper white, slight blue
    :base01 "#e8ecf2"  ; HSL(211° 28% 93%) - raised surfaces
    :base02 "#d4dbe6"  ; HSL(211° 28% 87%) - selections
    :base03 "#8896a8"  ; HSL(211° 18% 60%) - comments
    :base04 "#5c6b7d"  ; HSL(211° 16% 43%) - dark foreground
    :base05 "#2d3848"  ; HSL(211° 26% 23%) - default foreground
    :base06 "#1d2530"  ; HSL(211° 28% 15%) - darker text
    :base07 "#0f1318"  ; HSL(211° 24% 8%)  - darkest
    :base08 "#0550ae"  ; HSL(211° 95% 35%) - variables
    :base09 "#0969da"  ; HSL(211° 94% 45%) - integers
    :base0A "#1f6feb"  ; HSL(211° 86% 53%) - classes
    :base0B "#218bff"  ; HSL(211° 100% 57%) - strings (hero)
    :base0C "#54aeff"  ; HSL(211° 100% 66%) - support
    :base0D "#0550ae"  ; HSL(211° 95% 35%) - functions
    :base0E "#0969da"  ; HSL(211° 94% 45%) - keywords
    :base0F "#1b4b91") ; HSL(211° 70% 34%) - deprecated
  "Base16 Maas Neoform palette.")

(deftheme base16-maas-neoform)
(base16-theme-define 'base16-maas-neoform base16-maas-neoform-theme-colors)
(provide-theme 'base16-maas-neoform)
(provide 'base16-maas-neoform-theme)

;;; base16-maas-neoform-theme.el ends here
