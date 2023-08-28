# ************************************************************************
# R Code [S1E03] "Les taux d'intérêts: ----
# Est-ce qu'ils suivent les instructions du directeur?"
# ************************************************************************
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
#
# Feel free to copy, adapt, and use this code for your own purposes at
# your own risk.
#
# Matthias Spichiger, 2023 (matthias.spichiger@bluewin.ch)
# Based on Daniel Kaufmann, 2020 (daniel.kaufmann@unine.ch)
# ************************************************************************
library(tsbox)
library(tidyverse)
library(xts)
library(ggtext)

startDate <- "2000-01-01"
endDate   <- round_date(x = today(), unit = "month")

# ************************************************************************
# Download the data ----
# ************************************************************************
# Source: https://data.snb.ch/fr/
#         Manual forecasts for march and April if not available (partly based on early
#         financial market information)

## Import the data ----
urls <- c(
  "https://data.snb.ch/api/cube/zimoma/data/csv/en",
  "https://data.snb.ch/api/cube/rendoblid/data/csv/en",
  "https://data.snb.ch/api/cube/zikreddet/data/csv/en",
  "https://data.snb.ch/api/cube/snboffzisa/data/csv/en",
  "https://data.snb.ch/api/cube/zikrepro/data/csv/en"
)

names <- c(
  "MarchéMonétaire.csv",
  "Obligations.csv",
  "TauxCredits.csv",
  "TauxDirecteur.csv",
  "TauxOperationsNouvelles.csv"
)

# Ideally wrap this in a tryCatch() statement
for (i in seq_along(names)) {
  download.file(url = urls[i], destfile = paste0("S01E03_TauxDirecteur/", names[i]), method = "curl", quiet = FALSE)
}

### MarchéMonétaire ----
Monet <- read_delim(file = "S01E03_TauxDirecteur/MarchéMonétaire.csv", delim = ";", skip = 2)

# Chose 1 day SARON and 3-month LIBOR
Monet <- Monet |>
  filter(D0 %in% c("SARON", "3M0")) |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(LIB3M = `3M0`)

Monet <- xts(x = Monet[, c("SARON", "LIB3M")], order.by = Monet$Date)

### Obligations ----
Oblig <- read_delim(file = "S01E03_TauxDirecteur/Obligations.csv", delim = ";", skip = 2)

# Select 10-year CHF government bonds, 8-year Swiss commercial bonds from industry, 8-year foreign CHF commercial bonds, AAA rated
Oblig <- Oblig |>
  filter(D0 %in% c("10J0", "IKH", "AAA")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Conf10 = `10J0`, Manuf8 = IKH, Foreign8 = AAA)

Oblig <- ts_frequency(xts(x = Oblig[, c("Conf10", "Manuf8", "Foreign8")], order.by = Oblig$Date), to = "month", aggregate = "mean", na.rm = TRUE)

### TauxCrédits ----
Credit2 <- read_delim(file = "S01E03_TauxDirecteur/TauxCredits.csv", delim = ";", skip = 2)

# Select current account advance facilities and investment loans with fixed interest rates
Credit2 <- Credit2 |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  filter(D0 %in% c("K", "FI")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Courant = K, Invest = FI)

Credit2 <- xts(x = Credit2[, c("Courant", "Invest")], order.by = Credit2$Date)


### TauxDirecteur ----
Direct <- read_delim(file = "S01E03_TauxDirecteur/TauxDirecteur.csv", delim = ";", skip = 2)

# Select SNB policy rate, lower and upper target range for 3-month CHF LIBOR
Direct <- Direct |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  filter(D0 %in% c("LZ", "UG0", "OG0")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Direct = LZ, Infer = UG0, Super = OG0)

Direct <- xts(x = Direct[, c("Direct", "Infer", "Super")], order.by = Direct$Date)

### TauxOperationsNouvelles ----
Credit <- read_delim(file = "S01E03_TauxDirecteur/TauxOperationsNouvelles.csv", delim = ";", skip = 2)

Credit <- Credit |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  filter(D0 == "M") |> # Select median
  filter(D1 %in% c("MV", "S1", "33")) |> # Select variable interest rate mortgages, savings deposits and term deposits with a minimum of 100 000 CHF and 3 month duration
  select(Date, D1, Value) |>
  pivot_wider(names_from = D1, values_from = Value) |>
  rename(Hypo = MV, Epargne = S1, Terme = `33`)

Credit <- xts(x = Credit[, c("Hypo", "Epargne", "Terme")], order.by = Credit$Date)

## Combine the data ----
Taux <- ts_span(
  ts_c(
    Monet, Oblig, Direct, Credit, Credit2
  ),
  start = startDate
)

# Before 2019, there was a target range instead of a policy rate. Take the middle of the
# target range
Taux <- Taux |> 
  ts_tbl() |> 
  pivot_wider(names_from = id, values_from = value) |> 
  mutate(Direct = if_else(is.na(Direct), (Super + Infer) / 2, Direct))

Taux <- xts(x = Taux[, -c(1)], order.by = Taux$time)


# ************************************************************************
# Create charts -----
# ************************************************************************

## Taux directeur vs SARON (LIBOR) ----
p <- ts_df(
  ts_c(
    `Taux directeur` = Taux$Direct,
    `Swiss Average Rate Overnight (SARON)` = ts_span(Taux$SARON, start = "2019-06-01"),
    `CHF Libor à trois mois` = ts_span(Taux$LIB3M, start = startDate, end = "2019-05-01")
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  theme_minimal() +
  labs(
    title = "Taux directeur et taux sur le marché monétaire (en %)",
    subtitle = "<span style = 'color: black;'>Taux directeur</span>, <span style = 'color: #1B9E77;'>CHF Libor à trois mois</span>,  <span style = 'color: #D95F02;'>Swiss Average Rate Overnight (SARON)</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E03_TauxDirecteur/TauxDirecteur.png", width = 8, height = 4)
graphics.off()

## Recession 2002 ----
p <- ts_df(
  ts_c(
    `Taux directeur`         = ts_span(Taux$Direct, start = startDate, end = "2004-01-01"),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, start = startDate, end = "2004-01-01"),
    `Entreprises à 8 ans`    = ts_span(Taux$Manuf8, start = startDate, end = "2004-01-01"),
    `Prêts hypothécairses`   = ts_span(Taux$Hypo, start = startDate, end = "2004-01-01"),
    `Dépôt d'épargnes`       = ts_span(Taux$Epargne, start = startDate, end = "2004-01-01")
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Taux d'intérêts pendant la recession 2001/2002 (en %)",
    subtitle = "<span style = 'color: #1B9E77;'>Conféderation à 10 ans</span>, <span style = 'color: #D95F02;'>Dépôt d'épargnes</span>, <span style = 'color: #7570B3;'>Enreprises à 8 ans</span>, <span style = 'color: #E7298A;'>Prêts hypothécaires</span>, <span style = 'color: black;'>Taux directeur</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "#E6AB02", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")
p

ggsave(filename = "S01E03_TauxDirecteur/AutresTaux2001.png", width = 8, height = 4)
graphics.off()


## Recession 2008 ----
p <- ts_df(
  ts_c(
    `Taux directeur` = ts_span(Taux$Direct, "2006-01-01", "2010-01-01"),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, "2006-01-01", "2010-01-01"),
    `Entreprises à 8 ans` = ts_span(Taux$Manuf8, "2006-01-01", "2010-01-01"),
    `Prêts hypothécaires` = ts_span(Taux$Hypo, "2006-01-01", "2010-01-01"),
    `Dépôts d'épargnes` = ts_span(Taux$Epargne, "2006-01-01", "2010-01-01")
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Taux d'intérêts pendant la recession 2008/2009 (en %)",
    subtitle = "<span style = 'color: #1B9E77;'>Conféderation à 10 ans</span>, <span style = 'color: #D95F02;'>Dépôt d'épargnes</span>, <span style = 'color: #7570B3;'>Enreprises à 8 ans</span>, <span style = 'color: #E7298A;'>Prêts hypothécaires</span>, <span style = 'color: black;'>Taux directeur</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_minimal() +
  ylab("") +
  xlab("") +
  geom_line(aes(), linewidth = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E03_TauxDirecteur/AutresTaux2008.png", width = 8, height = 4)
graphics.off()

# Taux négatifs
p <- ts_df(
  ts_c(
    `Taux directeur` = ts_span(Taux$Direct, "2013-01-01", "2017-01-01"),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, "2013-01-01", "2017-01-01"),
    `Entreprises à 8 ans` = ts_span(Taux$Manuf8, "2013-01-01", "2017-01-01"),
    `Prêts hypothécaires` = ts_span(Taux$Hypo, "2013-01-01", "2017-01-01"),
    `Dépôts d'épargnes` = ts_span(Taux$Epargne, "2013-01-01", "2017-01-01")
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Taux d'intérêts en territoire négatif (en %)",
    subtitle = "<span style = 'color: #1B9E77;'>Conféderation à 10 ans</span>, <span style = 'color: #D95F02;'>Dépôt d'épargnes</span>, <span style = 'color: #7570B3;'>Enreprises à 8 ans</span>, <span style = 'color: #E7298A;'>Prêts hypothécaires</span>, <span style = 'color: black;'>Taux directeur</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E03_TauxDirecteur/AutresTaux2015.png", width = 8, height = 4)
graphics.off()


## Crise Corona ----
p <- ts_df(
  ts_c(
    `Taux directeur` = ts_span(Taux$Direct, "2016-01-01", endDate),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, "2016-01-01", endDate),
    `Entreprises à 8 ans` = ts_span(Taux$Manuf8, "2016-01-01", endDate),
    `Prêts hypothécaires` = ts_span(Taux$Hypo, "2016-01-01", endDate),
    `Dépôts d'épargnes` = ts_span(Taux$Epargne, "2016-01-01", endDate)
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Taux d'intérêts pendant la crise Corona (en %)",
    subtitle = "<span style = 'color: #1B9E77;'>Conféderation à 10 ans</span>, <span style = 'color: #D95F02;'>Dépôt d'épargnes</span>, <span style = 'color: #7570B3;'>Enreprises à 8 ans</span>, <span style = 'color: #E7298A;'>Prêts hypothécaires</span>, <span style = 'color: black;'>Taux directeur</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "black", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) +
  guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) +
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E03_TauxDirecteur/AutresTauxCorona.png", width = 8, height = 4)
graphics.off()
# END