# maas.nix — light mode variations
# the biotech complement to ono-sendai's chrome
#
# "She was probably his closest equivalent to a partner,
#  certainly his equal when it came to the mechanics of
#  data extraction." — Neuromancer
{
  # Pure discipline: blue-tinted paper
  # Like reading schematics in the Maas Biolabs clean room
  neoform = {
    slug = "maas-neoform";
    name = "Maas Neoform";
    author = "b7r6";
    variant = "light";
    palette = {
      # Grayscale ramp — inverted, cool-tinted (211° hue preserved)
      base00 = "#f5f7fa"; # HSL(211° 28% 97%) - paper white, slight blue
      base01 = "#e8ecf2"; # HSL(211° 28% 93%) - raised surfaces
      base02 = "#d4dbe6"; # HSL(211° 28% 87%) - selections
      base03 = "#8896a8"; # HSL(211° 18% 60%) - comments (legible!)
      base04 = "#5c6b7d"; # HSL(211° 16% 43%) - dark foreground
      base05 = "#2d3848"; # HSL(211° 26% 23%) - default foreground
      base06 = "#1d2530"; # HSL(211° 28% 15%) - darker text
      base07 = "#0f1318"; # HSL(211° 24% 8%)  - darkest

      # Accents — shifted darker for light bg legibility
      base08 = "#0550ae"; # HSL(211° 95% 35%) - variables (was ice, now deep)
      base09 = "#0969da"; # HSL(211° 94% 45%) - integers
      base0A = "#1f6feb"; # HSL(211° 86% 53%) - classes (hero shifted)
      base0B = "#218bff"; # HSL(211° 100% 57%) - strings (the new hero)
      base0C = "#54aeff"; # HSL(211° 100% 66%) - support (old hero, works here)
      base0D = "#0550ae"; # HSL(211° 95% 35%) - functions
      base0E = "#0969da"; # HSL(211° 94% 45%) - keywords
      base0F = "#1b4b91"; # HSL(211° 70% 34%) - deprecated
    };
  };

  # Warm paper — less clinical, easier on eyes
  # The executive suite at Maas, wood paneling optional
  bioptic = {
    slug = "maas-bioptic";
    name = "Maas Bioptic";
    author = "b7r6";
    variant = "light";
    palette = {
      # Warm cream paper (slight yellow, ~40° hue)
      base00 = "#faf8f5"; # HSL(40° 33% 97%) - warm paper
      base01 = "#f0ece5"; # HSL(40° 28% 92%) - raised
      base02 = "#e2dcd2"; # HSL(35° 24% 86%) - selections
      base03 = "#8a8378"; # HSL(35° 9% 51%)  - comments
      base04 = "#5c574e"; # HSL(35° 10% 33%) - dark fg
      base05 = "#33302a"; # HSL(35° 12% 18%) - default fg
      base06 = "#201e1a"; # HSL(35° 11% 11%) - darker
      base07 = "#121110"; # HSL(30° 8% 7%)   - darkest

      # Blues still, but warmer context makes them pop differently
      base08 = "#0550ae";
      base09 = "#0969da";
      base0A = "#1f6feb";
      base0B = "#218bff";
      base0C = "#54aeff";
      base0D = "#0550ae";
      base0E = "#0969da";
      base0F = "#6e5494"; # slight purple for deprecated (organic feel)
    };
  };

  # Ghost protocol — low contrast for photosensitivity
  # Late night light mode, the 3am variant
  ghost = {
    slug = "maas-ghost";
    name = "Maas Ghost";
    author = "b7r6";
    variant = "light";
    palette = {
      # Light gray instead of white — gentler
      base00 = "#e8ecf0"; # HSL(211° 20% 92%) - soft gray
      base01 = "#dce2e9"; # HSL(211° 22% 89%)
      base02 = "#c8d1dc"; # HSL(211° 22% 82%)
      base03 = "#7a8a9c"; # HSL(211° 16% 55%) - comments
      base04 = "#556270"; # HSL(211° 14% 39%)
      base05 = "#384452"; # HSL(211° 22% 27%) - default fg
      base06 = "#252e38"; # HSL(211° 22% 18%)
      base07 = "#151a20"; # HSL(211° 20% 10%)

      # Muted accents — less aggressive
      base08 = "#3272b5"; # softer than pure
      base09 = "#4080c4";
      base0A = "#4d8fd4";
      base0B = "#5a9de3";
      base0C = "#6aabef";
      base0D = "#3272b5";
      base0E = "#4080c4";
      base0F = "#5c6e85";
    };
  };

  # High contrast clinical — when you need to SEE
  # QA lab, full spectrum lighting, no hiding
  tessier = {
    slug = "maas-tessier";
    name = "Maas Tessier-Ashpool";
    author = "b7r6";
    variant = "light";
    palette = {
      # Pure white, maximum contrast
      base00 = "#ffffff"; # true white
      base01 = "#f0f3f6";
      base02 = "#d8e0e8";
      base03 = "#6b7a8a"; # comments still readable
      base04 = "#424d5b";
      base05 = "#1a2028"; # near-black text
      base06 = "#0d1117";
      base07 = "#000000"; # true black available

      # Maximum saturation accents
      base08 = "#0349b4"; # deep
      base09 = "#0758c9";
      base0A = "#0066e0";
      base0B = "#0078ff"; # vivid
      base0C = "#2090ff";
      base0D = "#0349b4";
      base0E = "#0758c9";
      base0F = "#7c3aed"; # purple for deprecated (stands out)
    };
  };
}
