-----------------------------------------------------------------------------------------------
Matthias Spichiger - matthias.spichiger@bluewin.ch

Based on Daniel Kaufmann, émissions "Macro-éCHo", Université de Neuchâtel

daniel.kaufmann@unine.ch

readme.txt for replication files
-----------------------------------------------------------------------------------------------
Changes
I use an R project to make the file paths shorter and independent from the user [Bryan, Jennifer, et al.](https://rstats.wtf/projects).

I use the tidyverse to import the data [Wickham, Hadley](https://r4ds.hadley.nz/data-import), hence
base::read.csv() becomes readr::read_delim() and xlsx::read.xlsx() becomes readxl::read_excel().

Note that the tidyverse functions load the data as tibble instead of data.frame objects. 
When a single column of a tibble is selected, it remains a column-vector and is not converted to a row-vector, meaning you have to use dplyr::pull() to convert it first.

[Core tidyverse](https://www.tidyverse.org/packages/) packages (dplyr, forcats, ggplot2, lubridate, purrr, readr, stringr, tibble, tidyr) are loaded with library(tidyverse).

Packages that are not used in the scripts are not loaded.

If the links to the data have changed they have been updated.

tsbox::ts_ggplot() is not working and has been replaced with ggplot2::ggplot().

To create dataframes for ggplot2::ggplot(), time series objects are collected within tsbox::ts_c() and converted to a data frame in long format
with ts_df().

If the time stamps of two series do not match, the series are converted from xts to ts objects in order to make implicit missing values explicit as NAs.
Before plotting, those NAs are deleted with na.omit8).

In all plots, I include the years on the x-axes, and I add a dashed horizontal line at zero. 
The data source and the creator ([@econmaett](https://twitter.com/econmaett)) are included in the caption.

Inside of ggplot functions, the deprecated "size"-argument is replaced with "linewidth" [Pedersen, Thomas Lin](https://www.tidyverse.org/blog/2022/08/ggplot2-3-4-0-size-to-linewidth/).

When possible, I include the legends in the title, using the [ggtext](https://wilkelab.org/ggtext/) package.

I also replaced ----- with **** so that the dashes indicate the document outline (Ctrl + Shift + O).

I use the [styler](https://styler.r-lib.org/) package to format the scripts according to the [tidyverse style guide](https://style.tidyverse.org/).


Notes
-----------------------------------------------------------------------------------------------
These files replicate the charts used in my video series "Macro-éCHo". All data sources can be 
inferred from the codes. All files can be freely downloaded and distributed. Use them at
your own risk with no guarantee or maintenance. If you spot an error, thanks for letting me know.

The videos are available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg

Episodes Season 1
-----------------------------------------------------------------------------------------------
S01E01: Prime de risque: "La courbe de fièvre de l'économie Suisse"
S01E02: Baromètre conjoncturel mondial: "Les saints de glace jettent un froid sur la Suisse"
S01E03: Le taux dirécteur: "Les taux d'intérêts: Est-ce qu'ils les instructions du dirécteur?"
S01E04: Les prix: "Les prix baissent en Suisse – C’est top n’est-ce pas?"
S01E05: Prime de risque Pt2: "La courbe de fièvre de l'économie Suisse - Partie 2"
	Note that the files for this episode are available on github.com/dankaufmann/f-curve