;;; base16-ono-sendai-tuned-theme.el --- Ono-Sendai Blue Tuned -*- lexical-binding: t; -*-
;;
;; Author: b7r6 / opus-4
;; URL: https://github.com/b7r6/hypermodern-emacs

(require 'base16-theme)

(defvar base16-ono-sendai-tuned-theme-colors
  '(:base00 "#191c20"  ; HSL(211° 12% 11%) - OLED-safe background
            :base01 "#1f232a"  ; HSL(211° 16% 14%) - raised surfaces
            :base02 "#2a3039"  ; HSL(211° 17% 19%) - selections
            :base03 "#3a424f"  ; HSL(211° 15% 28%) - comments
            :base04 "#6b7689"  ; HSL(211° 12% 48%) - dark foreground
            :base05 "#c5d0dd"  ; HSL(211° 28% 81%) - default foreground
            :base06 "#dce3ec"  ; HSL(211° 32% 89%) - light foreground
            :base07 "#f0f4f8"  ; HSL(211° 36% 95%) - light background
            :base08 "#b6e3ff"  ; HSL(201° 100% 86%) - ice blue
            :base09 "#80ccff"  ; HSL(201° 100% 75%) - sky blue
            :base0A "#54aeff"  ; HSL(211° 100% 66%) - HERO: electric blue
            :base0B "#218bff"  ; HSL(211° 100% 57%) - deep blue
            :base0C "#0969da"  ; HSL(211° 94% 45%) - matrix blue
            :base0D "#4d9fff"  ; HSL(211° 100% 65%) - link blue
            :base0E "#6cb6ff"  ; HSL(211° 100% 71%) - soft electric
            :base0F "#1f6feb") ; HSL(211° 86% 53%) - corp blue
  "Base16 Ono-Sendai Blue Tuned palette.")

(deftheme base16-ono-sendai-tuned)
(base16-theme-define 'base16-ono-sendai-tuned base16-ono-sendai-tuned-theme-colors)
(provide-theme 'base16-ono-sendai-tuned)
(provide 'base16-ono-sendai-tuned-theme)

;;; base16-ono-sendai-tuned-theme.el ends here
