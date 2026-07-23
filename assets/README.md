# Assets

This directory contains font assets used by the plotting scripts.

## Fonts

### DankMono

- `DankMono-Regular.otf` — Regular weight, used as the default body/plot text font.
- `DankMono-Italic.otf` — Italic weight, used for emphasized text in plots.
- `DankMono-Bold.otf` — Bold weight, used for titles and highlighted labels.

DankMono is a monospaced typeface. To use it in R plots, load the font files with the `sysfonts` / `showtext` packages and set the family name to `"DankMono"`.

Example:

```r
library(sysfonts)
library(showtext)

font_add(family = "DankMono",
         regular = "assets/DankMono-Regular.otf",
         bold = "assets/DankMono-Bold.otf",
         italic = "assets/DankMono-Italic.otf")
showtext_auto()

par(family = "DankMono")
```

## License

**The font files in this directory are NOT part of the project's source code and are NOT covered by the same license as the code in this repository.**

- `DankMono-Regular.otf`, `DankMono-Italic.otf`, and `DankMono-Bold.otf` are commercial fonts owned by their respective foundry/copyright holder.
- These files are included here for local rendering convenience only.
- You must obtain your own valid license to use or redistribute these fonts. Do not redistribute them unless you are authorized to do so.
- If you do not have a license, remove these font files or replace them with a freely licensed alternative.
