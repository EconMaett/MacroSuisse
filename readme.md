# Macro-éCHo

Matthias Spichiger - matthias.spichiger@bluewin.ch
Based on Daniel Kaufmann, émissions "Macro-éCHo", Université de Neuchâtel
daniel.kaufmann@unine.ch
`readme.md` for replication files


## Changes

- Following [Bryan, Jennifer, et al.](https://rstats.wtf/projects), I use an R project to make the file paths shorter and independent from the user .

- Following [Wickham, Hadley](https://r4ds.hadley.nz/data-import), I use the `tidyverse` to import the data , hence `base::read.csv()` became `readr::read_delim()` and `xlsx::read.xlsx()` became `readxl::read_excel()`.

  Note that the `tidyverse` functions load data as `tibble` instead of `data.frame` objects. 
  When a single column of a `tibble` is selected, it remains a column-vector and is not transposed to a row-vector, meaning you have to use `dplyr::pull()` before applying functions that use row-vectors as input.

- The [Core tidyverse](https://www.tidyverse.org/packages/) packages (`dplyr`, `forcats`, `ggplot2`, `lubridate`, `purrr`, `readr`, `stringr`, `tibble`, and `tidyr`) are loaded in one go with `library(tidyverse)`.
  Note that the non-core `tidyverse` package `readxl` has to be loaded separately with `load(readxl)`
  Packages that are not used in the scripts are not loaded.

- If the links to the data have changed they have been updated.

- `tsbox::ts_ggplot()` is not working and has been replaced with `ggplot2::ggplot()`.

To create data frames for `ggplot2::ggplot()`, time series objects are collected within `tsbox::ts_c()` and converted to a data frame in long format with `tsbox::ts_df()`.

- If the time stamps of two series do not match, the series are converted from `xts` to `ts` objects in order to make implicit missing values explicit as `NA`s.
  Before plotting, those `NA`s are deleted with `base::na.omit()`.

- In all plots, I include the years on the x-axes, and I add a dashed horizontal line at zero. 

- The data source and the creator ([@econmaett](https://twitter.com/econmaett)) are included in the caption.

- Inside of ggplot functions, the deprecated `size`-argument is replaced with `linewidth` (See [Pedersen, Thomas Lin](https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/)).

- Legends are included in the title, using the [ggtext](https://wilkelab.org/ggtext/) package.

- I replaced `----` with `****` so that the dashes indicate the **document outline** (`Ctrl + Shift + O`).

- I use the [styler](https://styler.r-lib.org/) package to format the scripts according to the [tidyverse style guide](https://style.tidyverse.org/).


## Notes

These files replicate the charts used in my video series "Macro-éCHo". All data sources can be inferred from the codes. All files can be freely downloaded and distributed. Use them at your own risk with no guarantee or maintenance. If you spot an error, thanks for letting me know.

The videos are available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg

## Episodes Season 1

- S01E01: Prime de risque: "La courbe de fièvre de l'économie Suisse"
- S01E02: Baromètre conjoncturel mondial: "Les saints de glace jettent un froid sur la Suisse"
- S01E03: Le taux dirécteur: "Les taux d'intérêts: Est-ce qu'ils les instructions du dirécteur?"
- S01E04: Les prix: "Les prix baissent en Suisse – C’est top n’est-ce pas?"
- S01E05: Prime de risque Pt2: "La courbe de fièvre de l'économie Suisse - Partie 2"
	Note that the files for this episode are available on github.com/dankaufmann/f-curve
