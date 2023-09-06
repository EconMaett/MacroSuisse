# ************************************************************************
# R Code [S1E03] "Les taux d'intérêts: ----
# Est-ce qu'ils suivent les instructions du directeur?"
# ************************************************************************
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************
library(tsbox)
library(tidyverse)
library(xts)
library(ggtext)

start_date <- "2000-01-01"
end_date   <- round_date(x = today(), unit = "month")
chrecdp    <- read_csv(file = "Recession-Dates/Recession-Dates_OECD_CH_Daily_Midpoint.csv")

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
  "Marche-Monetaire.csv",
  "Obligations.csv",
  "Taux-Credits.csv",
  "Taux-Directeur.csv",
  "Taux-Operations-Nouvelles.csv"
)

# Ideally wrap this in a tryCatch() statement
for (i in seq_along(names)) {
  download.file(url = urls[i], destfile = paste0("S01E03_Taux-Directeur/", names[i]), method = "curl", quiet = FALSE)
}

### MarchéMonétaire ----
Monet <- read_delim(file = "S01E03_Taux-Directeur/Marche-Monetaire.csv", delim = ";", skip = 2)

# Chose 1 day SARON and 3-month LIBOR
Monet <- Monet |>
  filter(D0 %in% c("SARON", "3M0")) |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(LIB3M = `3M0`)

Monet <- xts(x = Monet[, c("SARON", "LIB3M")], order.by = Monet$Date)

### Obligations ----
Oblig <- read_delim(file = "S01E03_Taux-Directeur/Obligations.csv", delim = ";", skip = 2)

# Select 10-year CHF government bonds, 8-year Swiss commercial bonds from industry, 8-year foreign CHF commercial bonds, AAA rated
Oblig <- Oblig |>
  filter(D0 %in% c("10J0", "IKH", "AAA")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Conf10 = `10J0`, Manuf8 = IKH, Foreign8 = AAA)

Oblig <- ts_frequency(xts(x = Oblig[, c("Conf10", "Manuf8", "Foreign8")], order.by = Oblig$Date), to = "month", aggregate = "mean", na.rm = TRUE)

### TauxCrédits ----
Credit2 <- read_delim(file = "S01E03_Taux-Directeur/Taux-Credits.csv", delim = ";", skip = 2)

# Select current account advance facilities and investment loans with fixed interest rates
Credit2 <- Credit2 |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  filter(D0 %in% c("K", "FI")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Courant = K, Invest = FI)

Credit2 <- xts(x = Credit2[, c("Courant", "Invest")], order.by = Credit2$Date)


### TauxDirecteur ----
Direct <- read_delim(file = "S01E03_Taux-Directeur/Taux-Directeur.csv", delim = ";", skip = 2)

# Select SNB policy rate, lower and upper target range for 3-month CHF LIBOR
Direct <- Direct |>
  mutate(Date = as.Date(paste0(Date, "-01"))) |>
  filter(D0 %in% c("LZ", "UG0", "OG0")) |>
  pivot_wider(names_from = D0, values_from = Value) |>
  rename(Direct = LZ, Infer = UG0, Super = OG0)

Direct <- xts(x = Direct[, c("Direct", "Infer", "Super")], order.by = Direct$Date)

### TauxOperationsNouvelles ----
Credit <- read_delim(file = "S01E03_Taux-Directeur/Taux-Operations-Nouvelles.csv", delim = ";", skip = 2)

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
  start = start_date
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
    `CHF Libor à trois mois` = ts_span(Taux$LIB3M, start = start_date, end = "2019-05-01")
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 5), breaks = seq(-1, 5, 1)) +
  scale_color_manual(
    breaks = c("Taux directeur", "Swiss Average Rate Overnight (SARON)", "CHF Libor à trois mois"),
    values = c("#374e8e", "#006d64", "#ac004f")
  ) +
  labs(
    title = "Taux directeur et taux sur le marché monétaire (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Taux directeur</span>, <span style = 'color: #006d64;'>CHF Libor à trois mois</span>,  <span style = 'color: #ac004f;'>Swiss Average Rate Overnight (SARON)</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E03_Taux-Directeur/Fig_Taux-Directeur.png", width = 8, height = 4)
graphics.off()

## Recession 2002 ----
p <- ts_df(
  ts_c(
    `Taux directeur`         = ts_span(Taux$Direct, start = start_date, end = "2004-01-01"),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, start = start_date, end = "2004-01-01"),
    `Entreprises à 8 ans`    = ts_span(Taux$Manuf8, start = start_date, end = "2004-01-01"),
    `Prêts hypothécairses`   = ts_span(Taux$Hypo, start = start_date, end = "2004-01-01"),
    `Dépôt d'épargnes`       = ts_span(Taux$Epargne, start = start_date, end = "2004-01-01")
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date(start_date), date("2004-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 5), breaks = seq(-1, 5, 1)) +
  scale_color_manual(
    breaks = c("Taux directeur", "Conféderation à 10 ans", "Entreprises à 8 ans", "Prêts hypothécairses", "Dépôt d'épargnes"), 
    values = c("#374e8e", "#006d64", "#ac004f", "#df7c18", "#a07bde")
    ) +
  labs(
    title = "Taux d'intérêts pendant la recession 2001/2002 (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Taux directeur</span>, <span style = 'color:#006d64;'>Conféderation à 10 ans</span>, <span style = 'color: #ac004f;'>Enreprises à 8 ans</span>, <span style = 'color: #df7c18;'>Prêts hypothécaires</span>, <span style = 'color: #a07bde;'>Dépôt d'épargnes</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E03_Taux-Directeur/Fig_Autres-Taux-2001.png", width = 8, height = 4)
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
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date("2006-01-01"), date("2010-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 5), breaks = seq(-1, 5, 1)) +
  scale_color_manual(
    breaks = c("Taux directeur", "Conféderation à 10 ans", "Entreprises à 8 ans", "Prêts hypothécaires", "Dépôts d'épargnes"), 
    values = c("#374e8e", "#006d64", "#ac004f", "#df7c18", "#a07bde")
  ) +
  labs(
    title = "Taux d'intérêts pendant la recession 2008/2009 (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Taux directeur</span>, <span style = 'color:#006d64;'>Conféderation à 10 ans</span>, <span style = 'color: #ac004f;'>Enreprises à 8 ans</span>, <span style = 'color: #df7c18;'>Prêts hypothécaires</span>, <span style = 'color: #a07bde;'>Dépôt d'épargnes</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E03_Taux-Directeur/Fig_Autres-Taux-2008.png", width = 8, height = 4)
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
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date("2013-01-01"), date("2017-01-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 5), breaks = seq(-1, 5, 1)) +
  scale_color_manual(
    breaks = c("Taux directeur", "Conféderation à 10 ans", "Entreprises à 8 ans", "Prêts hypothécaires", "Dépôts d'épargnes"), 
    values = c("#374e8e", "#006d64", "#ac004f", "#df7c18", "#a07bde")
  ) +
  labs(
    title = "Taux d'intérêts en territoire négatif (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Taux directeur</span>, <span style = 'color:#006d64;'>Conféderation à 10 ans</span>, <span style = 'color: #ac004f;'>Enreprises à 8 ans</span>, <span style = 'color: #df7c18;'>Prêts hypothécaires</span>, <span style = 'color: #a07bde;'>Dépôt d'épargnes</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E03_Taux-Directeur/Fig_Autres-Taux-2015.png", width = 8, height = 4)
graphics.off()


## Crise Corona ----
p <- ts_df(
  ts_c(
    `Taux directeur` = ts_span(Taux$Direct, "2016-01-01", end_date),
    `Conféderation à 10 ans` = ts_span(Taux$Conf10, "2016-01-01", end_date),
    `Entreprises à 8 ans` = ts_span(Taux$Manuf8, "2016-01-01", end_date),
    `Prêts hypothécaires` = ts_span(Taux$Hypo, "2016-01-01", end_date),
    `Dépôts d'épargnes` = ts_span(Taux$Epargne, "2016-01-01", end_date)
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date("2016-01-01"), date(end_date)), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 5), breaks = seq(-1, 5, 1)) +
  scale_color_manual(
    breaks = c("Taux directeur", "Conféderation à 10 ans", "Entreprises à 8 ans", "Prêts hypothécaires", "Dépôts d'épargnes"), 
    values = c("#374e8e", "#006d64", "#ac004f", "#df7c18", "#a07bde")
  ) +
  labs(
    title = "Taux d'intérêts pendant la crise Corona (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Taux directeur</span>, <span style = 'color:#006d64;'>Conféderation à 10 ans</span>, <span style = 'color: #ac004f;'>Enreprises à 8 ans</span>, <span style = 'color: #df7c18;'>Prêts hypothécaires</span>, <span style = 'color: #a07bde;'>Dépôt d'épargnes</span>",
    caption = "@econmaett. Source de données: Banque Nationale Suisse (BNS).",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E03_Taux-Directeur/Fig_Autres-Taux-Corona.png", width = 8, height = 4)
graphics.off()

# END