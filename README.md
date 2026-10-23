# langstat

_A lightweight R-based tool for analyzing GitHub repository language statistics with automated visualization._

This project was originally part of my major research from my school work, and I decided to open source it for the community. This is also my first R project on GitHub!

You can build your own language statistics visualization by forking this repository. Welcome to star, contribute and any suggestions are welcome.

## Examples

They are the examples output for my [GitHub profile](https://github.com/mingcheng). For more example and formats, please check the [data branch](https://github.com/mingcheng/langstat/tree/refs/heads/data).

![piechart example](https://raw.githubusercontent.com/mingcheng/langstat/refs/heads/data/data/mingcheng/latest.png)

![treemap example](https://raw.githubusercontent.com/mingcheng/langstat/refs/heads/data/data/mingcheng/treemap_latest.png)

## Features

- Fetches repository data via GitHub API
- Calculates weighted language distribution based on:
  - Push recency with a one-year half-life
  - Bounded popularity signals (stars and fork counts)
  - Archived status and repeated use across original repositories
- Excludes forked, disabled and empty repositories
- Generates multiple visualizations, both in SVG and PNG formats:
  - Pie Charts
  - Treemaps
- Automated monthly updates via GitHub Actions
- Dank Mono typography with a shared soft-color palette for both chart types

### Scoring model

Only original, enabled, non-empty repositories with a known primary language
are scored. Forked repositories are **completely excluded**, regardless of their
popularity or last push. Here, "empty" means GitHub reports `size == 0`.

| Factor            | Range / rule                                | Purpose                                                               |
| ----------------- | ------------------------------------------- | --------------------------------------------------------------------- |
| Recency           | `2 ^ (-days_since_push / 365)`, from 0 to 1 | One year without a push halves the contribution; two years leaves 25% |
| Archived status   | 1 for active, 0.5 for archived              | Retain past work with a reduced contribution                          |
| Base contribution | 0.70                                        | Usage matters more than popularity                                    |
| Stars             | `0.20 * min(log1p(stars) / log1p(200), 1)`  | Diminishing returns, capped at 200 stars                              |
| Fork count        | `0.10 * min(log1p(forks) / log1p(50), 1)`   | Smaller popularity signal, capped at 50 forks                         |

```text
repository_score = recency * archived_weight * (0.70 + star_contribution + fork_contribution)
language_score   = sum(repository_score)
percentage       = language_score / sum(all_language_scores) * 100
```

Every repository contributes between 0 and 1. Popularity can add at most 0.30
before the activity discounts, so a popular project cannot outweigh two equally
recent, non-archived projects solely through its stars and forks. Language totals
can exceed 1 because they sum multiple repositories; only each repository's
contribution is bounded. Repository count is already reflected in that sum, so
there is no additional count bonus.

Future pushes are treated as today. Missing or invalid `pushed_at` falls back
to `updated_at` with a warning; if neither date is usable, the repository receives
zero weight with a warning. Missing eligibility metadata or invalid numeric
counts cause an explicit error rather than silently treating an unknown repo as
original. Refresh older cached data if required fields are missing. Analysis
stops if no positive language scores remain.

CSV output includes `base_score`, `star_score`, `fork_score`,
`archived_repo_count` and `effective_repo_count` (the sum of recency times archived
weight). `final_score` equals `total_score` and the sum of the three score
components; the old `repo_count_score` bonus column is removed.
`avg_days_since_update` retains its name for compatibility but now measures the
selected activity date. Chart subtitles show scored / fetched repository counts.

Both charts combine languages below 1% into a neutral-colored **Other** block,
using unrounded scores for the cutoff. The treemap keeps at most 12 individual
languages and groups any remaining languages into Other too. Their scores are
preserved, not dropped or redistributed. Zero-score languages have no visible
area, and CSV output always retains the full language breakdown.

These coefficients are transparent heuristics, not a measure of personal
proficiency. A repository's last push may be from another contributor, and its
primary language does not describe every technology it uses. Percentages are
relative: if all stacks age equally, their shares remain unchanged.

## Usage

### Local Execution

```bash
# Set environment variables
export GITHUB_USERNAME="your_username"

# Run analysis
Rscript R/main.R
```

### Tests

```bash
Rscript tests/run_tests.R
```

### GitHub Actions

The workflow runs automatically on the 1st of each month, or can be triggered manually via workflow dispatch. Results are committed to the `data` branch with the following structure:

```
data/{username}/
├── raw_YYYYMMDD.json       # Raw API response
├── plotted_YYYYMMDD.csv    # Statistical summary
├── piechart_YYYYMMDD.svg   # Date-stamped pie chart
├── piechart_YYYYMMDD.png
├── treemap_YYYYMMDD.svg    # Date-stamped treemap
├── treemap_YYYYMMDD.png
├── latest.svg              # Latest pie chart
├── latest.png
├── treemap_latest.svg      # Latest treemap
└── treemap_latest.png
```

You can link to these images in your GitHub README or personal website to showcase your language statistics. The `latest.png` and `treemap_latest.png` (or svg) files always point to the most recent visualizations.

## Requirements

- R >= 4 (tested on 4.5.1)

## Installation

### System dependencies (Ubuntu/Debian)

Some R packages need system libraries for HTTP, font rendering and image I/O:

```bash
sudo apt-get update
sudo apt-get install -y --no-install-recommends \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libfontconfig1-dev \
  libharfbuzz-dev \
  libfribidi-dev \
  libfreetype6-dev \
  libpng-dev \
  libtiff5-dev \
  libjpeg-dev
```

On Windows the binary packages on CRAN include the required libraries.

### System dependencies (macOS)

On macOS, first install the Xcode Command Line Tools:

```bash
xcode-select --install
```

Then install the required libraries via Homebrew:

```bash
brew install \
  curl-openssl \
  openssl \
  libxml2 \
  fontconfig \
  harfbuzz \
  fribidi \
  freetype \
  libpng \
  libtiff \
  jpeg
```

If you are using the CRAN binary R package, many of these dependencies are already bundled and only the Xcode Command Line Tools may be necessary.

### R packages

Install the required R packages from CRAN:

```r
packages <- c(
  "httr", "jsonlite", "dplyr", "showtext", "sysfonts",
  "ggplot2", "treemapify", "svglite"
)
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages, repos = "https://cloud.r-project.org", dependencies = NA)
}
```

Or install them directly:

```r
install.packages(
  c("httr", "jsonlite", "dplyr", "showtext", "sysfonts", "ggplot2", "treemapify", "svglite"),
  dependencies = NA
)
```

#### For users in mainland China

If downloading from the default CRAN mirror is slow or fails, use a domestic CRAN mirror, for example the TUNA mirror:

```r
install.packages(
  c("httr", "jsonlite", "dplyr", "showtext", "sysfonts", "ggplot2", "treemapify", "svglite"),
  repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/",
  dependencies = NA
)
```

Other commonly used domestic CRAN mirrors include:

- TUNA (Tsinghua): `https://mirrors.tuna.tsinghua.edu.cn/CRAN/`
- USTC: `https://mirrors.ustc.edu.cn/CRAN/`
- Aliyun: `https://mirrors.aliyun.com/CRAN/`

The visualization fonts are bundled in the `assets/` directory, so no extra font installation is required.

## Configuration

| Environment Variable                | Description                                                           | Default                    |
| ----------------------------------- | --------------------------------------------------------------------- | -------------------------- |
| `GITHUB_USERNAME`                   | Target username                                                       | `mingcheng`(Yes, it's me!) |
| `GITHUB_PER_PAGE`(Optional)         | Repos per API call                                                    | `100`                      |
| `GITHUB_TOKEN` (Optional)           | GitHub API authentication token                                       | Unauthenticated requests   |
| `TREEMAP_MIN_PERCENTAGE` (Optional) | Minimum language share shown in the treemap (0-100, decimals allowed) | `2`                        |
| `TREEMAP_MIN_FONT_SIZE` (Optional)  | Minimum treemap label size in points (positive, decimals allowed)     | `16`                       |
| `TREEMAP_MAX_FONT_SIZE` (Optional)  | Maximum treemap label size in points (must be at least the minimum)   | `32`                       |

For example, show only languages with at least 3.5% and use 18-34 pt labels:

```bash
TREEMAP_MIN_PERCENTAGE=3.5 TREEMAP_MIN_FONT_SIZE=18 TREEMAP_MAX_FONT_SIZE=34 Rscript R/main.R
```

You can also manually start the GitHub Actions process from the "Actions" tab in your repository, and then execute the analysis by clicking the "Run workflow" button.

The bundled visualization font is [Dank Mono](https://dank.sh).

## License

This project is licensed under the MIT License, see [LICENSE.md](LICENSE.md) for details.
