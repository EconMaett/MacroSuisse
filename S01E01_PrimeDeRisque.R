# ************************************************************************
# R Code [S1E01] "La courbe de fièvre de l'économie Suisse" ----
# ************************************************************************
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
#
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# Based on Daniel Kaufmann, 2020 (daniel.kaufmann@unine.ch)
# Université de Neuchâtel et KOF Centre de recherches conjoncturelles
# ************************************************************************
library(tidyverse) # Core tidyverse packages dplyr, forcats, ggplot2, lubridate, purrr, readr, stringr, tibble, tidyr
library(tsbox)
library(xts) # Builds on the zoo package
library(readxl) # Non-core tidyverse package; load separately
library(ggtext) # To include HTML code in ggplot elements

startDate <- "2007-06-01"

# ************************************************************************
# Compute risk premia ----
# ************************************************************************
# Qu'est-ce que c'est une prime de risque?
# C'est la différence entre le rendement d'une dette soumis à un risque
# de faillite (entreprises privées) et le rendement d'une dette sans risque
# (obligation de la Conféderation Suisse).

## Download the data ----
urls <- c(
  "https://www.bfs.admin.ch/bfsstatic/dam/assets/7966853/master",
  "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_gov_y.csv",
  "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_dom_non_gov_rating_sbi_y.csv"
)

names <- c("Defaults_OFS.xlsx", "ObligationsConf.csv", "ObligationsEnt.csv")

# Ideally, you would use a tryCatch statement here
for (i in seq_along(urls)) {
  download.file(url = urls[i], destfile = paste0("S01E01_PrimeDeRisque/", names[i]), mode = "wb", quiet = TRUE)
}

# Note: If you use readr::read_csv() instead of base::read.csv, the data are stored in tibble objects instead of data.frame objects
# Selecting a single column of a tibble results in a tibble, selecting a single column of a data.frame results in a vector
# You have to use pull() to store a single tibble column as a vector
Gov      <- read_delim(file = "S01E01_PrimeDeRisque/ObligationsConf.csv", delim = ";", skip = 4)
NonGov   <- read_delim(file = "S01E01_PrimeDeRisque/ObligationsEnt.csv", delim = ";", skip = 4)
Defaults <- read_excel(path = "S01E01_PrimeDeRisque/Defaults_OFS.xlsx", sheet = "T6.2.3.1", skip = 1)

# Beginning of default procedures (Ouverture des procedures de faillite)
# I calculate the growth rate of default procedures as a share of active firms in 2017
# https://www.bfs.admin.ch/bfs/de/home/statistiken/industrie-dienstleistungen/unternehmen-beschaeftigte/unternehmensdemografie/bestand-aktiver.html
myDate     <- as.Date(paste0(names(Defaults[, 2:ncol(Defaults)]), "-01-01"))
myDefaults <- as.numeric(Defaults[1, 2:ncol(Defaults)]) # Eröffnung Konkursverfahren
Faillite   <- ts_diff(xts(x = myDefaults, order.by = myDate)) / 1000 # Take first difference, divide by 1000

# Create date series
myDate1 <- dmy(Gov$DATE)
myDate2 <- dmy(NonGov$DATE)

# Create gov. bond yields
Gov1  <- xts(as.numeric(pull(Gov[, 2])), order.by = myDate1) # SBIGY1: SBI Dom Gov 1-3 Y
Gov3  <- xts(as.numeric(pull(Gov[, 3])), order.by = myDate1) # SBIGY3: SBI Dom Gov 3-7 Y
Gov7  <- xts(as.numeric(pull(Gov[, 7])), order.by = myDate1) # SG71Y:  SBI Dom Gov 7-10 Y
Gov10 <- xts(as.numeric(pull(Gov[, 8])), order.by = myDate1) # SG10Y:  SBI Dom Gov 10+ Y

# Create corporate bond yields AAA - BBB
BBB1  <- xts(as.numeric(pull(NonGov[, 3])), order.by = myDate2) # SDN13Y: SBI Dom Non-Gov AAA-BBB 1-3 Y
BBB3  <- xts(as.numeric(pull(NonGov[, 4])), order.by = myDate2) # SDN35Y: SBI Dom Non-Gov AAA-BBB 3-5 Y
BBB7  <- xts(as.numeric(pull(NonGov[, 6])), order.by = myDate2) # SDN71Y: SBI Dom Non-Gov AAA-BBB 7-10 Y
BBB10 <- xts(as.numeric(pull(NonGov[, 7])), order.by = myDate2) # SDN10Y: SBI Dom Non-Gov AAA-BBB 10+ Y

AA1  <- xts(as.numeric(pull(NonGov[, 15])), order.by = myDate2) # SAN13Y: SBI Dom Non-Gov AAA-AA 1-3 Y
AA3  <- xts(as.numeric(pull(NonGov[, 16])), order.by = myDate2) # SAN35Y: SBI Dom Non-Gov AAA-AA 3-5 Y
AA7  <- xts(as.numeric(pull(NonGov[, 18])), order.by = myDate2) # SAN71Y: SBI Dom Non-Gov AAA-AA 7-10 Y
AA10 <- xts(as.numeric(pull(NonGov[, 19])), order.by = myDate2) # SAN10Y: SBI Dom Non-Gov AAA-AA 10+ Y

# Compute implies probability of default
# i: risk-less interest rate
# x:  risk premium
# x = (1+i+x) - (1+i) -> We can calculate the risk premium as the rate of return between
#                        a risk-less investment and the corporate bond yield
RPBBB1 <- BBB1 - Gov1
RPAAA1 <- AA1  - Gov1

# ************************************************************************
# Create charts ----
# ************************************************************************
# Important dates; Bankruptcy of Lehman Brothers, removal of CHF-EUR peg, COVID-19 pandemic
myLines <- c(as.numeric(as.Date("2008-09-15")), as.numeric(as.Date("2015-01-15")), as.numeric(as.Date("2020-02-01")))

# the function ts_ggplot() has problems, we therefore create a data frame and
# use regular ggplot2 functions instead

# ************************************************************************
# Government & investment-grade corporate bond yields ----
# ************************************************************************
p <- ts_df(
  ts_span(
    ts_c(
      `Confédération` = Gov1,
      `Entreprises (notation AA-AAA)` = AA1,
      `Entreprises (notation BBB-AAA)` = BBB1
      ),
    start = startDate
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(
    title = "Rendements des obligations à 1 ans (en %)",
    subtitle = "<span style = 'color: black;'>Confédération</span>, <span style = 'color: #1B9E77;'>Entreprises (AA-AAA)</span>, <span style = 'color: #D95F02;'>Entreprises (BBB-AAA)</span>",
    caption = "@econmaett. Source de données: SIX",
    x = "", y = ""
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("black", "#1B9E77", "#D95F02")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = ggtext::element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E01_PrimeDeRisque/Obligations.png", width = 5, height = 4)
graphics.off()

# ************************************************************************
# risk premia between govenment and corporate bonds ----
# ************************************************************************
p <- ts_df(
  ts_span(
    ts_c(
      `Notation AA-AAA`  = ts_span(RPAAA1, startDate),
      `Notation BBB-AAA` = ts_span(RPBBB1, startDate)
      ),
    start = startDate
    )
  ) |>
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  geom_vline(xintercept = myLines, colour = "blue", linewidth = 1, alpha = 0.5) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  theme_minimal() +
  labs(
    title = "Prime de risque à 1 ans (en pp)",
    subtitle = "<span style = 'color: #1B9E77;'>Entreprises AA-AAA</span>, <span style = 'color: #D95F02;'>Entreprises BBB-AAA</span>", 
    caption = "@econmaett. Source de données: SIX",
    x = "", y = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = ggtext::element_markdown(), legend.position = "none")

p <- p + geom_text(x = myLines[1], y = 1.3, label = "Faillite Lehman", colour = "blue", angle = 90, vjust = -1)
p <- p + geom_text(x = myLines[2], y = 1.0, label = "Fin du taux plancher", colour = "blue", angle = 90, vjust = -1)
p <- p + geom_text(x = myLines[3], y = 1.0, label = "Crise Corona", colour = "blue", angle = 90, vjust = -1)

p
# Le risque d’être en faillite, et donc la prime de risque, 
# augmente dans des crises économiques. 
# Une «courbe de fièvre» avec laquelle on peut rapidement détecter des crisés économiques
ggsave(filename = "S01E01_PrimeDeRisque/PrimesDeRisque.png", width = 5, height = 4)
graphics.off()


# ************************************************************************
# Openings of bankruptcy procedures and risk premia ----
# ************************************************************************
# The author used xts objects. these do not know implicit
# time series. We therefore use ts objects, which always make
# implicit missing values explicit as NAs.
# That way we can combine RPBB1, which has observations every trading day,
# and Faillite, which has one observation every month, in a single data frame.
p <- ts_df(
  ts_span(
    ts_c(
      `Prime de risque (notation BBB-AAA, en pp)` = ts_ts(RPBBB1),
      `Ouverture proc. de faillite (variation, en 1000 ouvertures par an)` = ts_ts(Faillite)
      ),
    start = startDate
    )
  ) |> 
  na.omit() |> # Remove the missing observations for the plot
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  theme_minimal() +
  labs(
    title = "Relation avec des ouvertures de procédure de faillite",
    subtitle = "<span style = 'color: #D95F02;'>Prime de risque (BBB-AAA, en pp)</span>, <span style = 'color: black;'>Ouvert. (variat., en 1000 pa)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS)",
    x = "", y = ""
  ) +
  scale_color_manual(values = c("black", "#D95F02")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = ggtext::element_markdown(), legend.position = "none")

p
# Lien avec la variation des ouvertures de procédures de faillite.
# Les informations des marchés financiers sont disponible chaque jour!
# Des autres statistiques sont publié avec des retards important.
ggsave(filename = "S01E01_PrimeDeRisque/ProcFaillitePrime.png", width = 5, height = 4)
graphics.off()
# END