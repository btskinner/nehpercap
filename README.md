# Lifetime per capita contributions to NEH appropriations: 1966-2024

This script uses public data to make a figure showing the cumulative per capita
contribution to National Endowment for the Humanities.

## To run

``` shell
cd scripts
Rscript appropriations.R
```

## Figure

![](https://raw.githubusercontent.com/btskinner/nehpercap/main/figures/neh_appropriations_pc_cumulative.png)

## NOTES

All data are publicly available.

The script uses the [`fredr`](https://sboysel.github.io/fredr/index.html)
package, which requires an API key. [See the package
manual](https://sboysel.github.io/fredr/articles/fredr.html) for information
about getting and storing an API key.

