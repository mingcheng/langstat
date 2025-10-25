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
  - Repository update recency
  - Stars and forks count
  - Repository count per language
- Generates multiple visualizations:
  - Pie charts (SVG & PNG formats)
  - Treemaps (SVG & PNG formats)
- Automated monthly updates via GitHub Actions

We use the [Sometype Mono font](https://monospacedfont.com/) for better representation in the charts, thanks for providing such a great free font!

## Usage

### Local Execution

```bash
# Set environment variables
export GITHUB_USERNAME="your_username"

# Run analysis
Rscript R/main.R
```

### GitHub Actions

The workflow runs automatically on the 1st of each month, or can be triggered manually via workflow dispatch. Results are committed to the `data` branch with the following structure:

```
data/{username}/
├── raw_YYYYMMDD.json       # Raw API response
├── plotted_YYYYMMDD.csv    # Statistical summary
├── chart_YYYYMMDD.svg      # Date-stamped pie chart
├── chart_YYYYMMDD.png
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
- R packages: `httr`, `jsonlite`, `dplyr`, `magrittr`, `showtext`, `ggplot2`, `treemapify`, `svglite`

## Configuration

| Environment Variable        | Description        | Default                    |
| --------------------------- | ------------------ | -------------------------- |
| `GITHUB_USERNAME`           | Target username    | `mingcheng`(Yes, it's me!) |
| `GITHUB_PER_PAGE`(Optional) | Repos per API call | `100`                      |

Tips: You can also manually start the GitHub Actions process from the "Actions" tab in your repository, and then execute the analysis by clicking the "Run workflow" button.

## License

MIT License, see [LICENSE.md](LICENSE.md) for details.
