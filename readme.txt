-----------------------------------------------------------------------------------------------
Matthias Spichiger - matthias.spichiger@bluewin.ch

Based on Daniel Kaufmann, émissions "Macro-éCHo", Université de Neuchâtel

daniel.kaufmann@unine.ch

readme.txt for replication files
-----------------------------------------------------------------------------------------------
Changes
I created an R project. Therefore the paths to the data and graphs become shorter and independent from the author.

I use tidyverse packages for data handling. 
Hence I load data with either readr::read_delim() instead of base::read.csv() or
readxl::read_excel() instead of xlsx::read.xlsx().

Sometimes the links to access the data have changed. I have updated those.

Because the tsbox::ts_ggplot() function is not working anymore, I have used ggplot2::ggplot() instead.
Note that sometimes I had to convert xts objects into ts objects before combining them with tsbox::ts_c()
to make implicit missing values visible as NAs, so that time series with different numbers of observations per period
can be combined in a single data frame.
Before plotting, I removed the NAs again.

I include more years on the x-axes in plots
I add the source of the data to the caption
I always add a zero-line to the plots
In ggplot functions, the argument "size" has been replaced with "linewidth".

When possible, I include the legends in the title.

I also replaced ----- with **** so that the dashes may indicate the document outline.


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