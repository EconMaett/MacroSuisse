# ************************************************************************
# R Code [S1E01] "La courbe de fièvre de l'économie Suisse" ----
# ************************************************************************
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************
library(tidyverse)
library(tsbox)
library(xts)
library(readxl)
library(ggtext)

start_date <- "2007-06-01"
chrecdp    <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

# ************************************************************************
# Compute risk premia ----
# ************************************************************************

## Download the data ----
urls <- c(
  "https://www.bfs.admin.ch/bfsstatic/dam/assets/7966853/master",
  "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_gov_y.csv",
  "https://www.six-group.com/exchanges/downloads/indexdata/hsb_maturity_dom_non_gov_rating_sbi_y.csv"
)

names <- c("Defaults-OFS.xlsx", "Obligations-Confederation.csv", "Obligations-Entreprises.csv")

# Ideally, you would use a tryCatch statement here
for (i in seq_along(urls)) {
  download.file(url = urls[i], destfile = paste0("S01E01_Prime-de-Risque/", names[i]), mode = "wb", quiet = TRUE)
}

Gov      <- read_delim(file = "S01E01_Prime-de-Risque/Obligations-Confederation.csv", delim = ";", skip = 4)
NonGov   <- read_delim(file = "S01E01_Prime-de-Risque/Obligations-Entreprises.csv", delim = ";", skip = 4)
Defaults <- read_excel(path = "S01E01_Prime-de-Risque/Defaults-OFS.xlsx", sheet = "T6.2.3.1", skip = 1)

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
    start = start_date
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1.5, 3.5), breaks = seq(-1.5, 3.5, 0.5)) +
  scale_color_manual(
    breaks = c("Confédération", "Entreprises (notation AA-AAA)", "Entreprises (notation BBB-AAA)"), 
    values = c("#374e8e", "#006d64", "#ac004f")
    ) +
  labs(
    title = "Rendements des obligations à 1 ans (en %)",
    subtitle = "<span style = 'color: #374e8e;'>Confédération</span>, <span style = 'color: #006d64;'>Entreprises (AA-AAA)</span>, <span style = 'color: #ac004f;'>Entreprises (BBB-AAA)</span>",
    caption = "@econmaett. Source de données: SIX",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E01_Prime-de-Risque/Fig_Obligations.png", width = 8, height = 4)
graphics.off()

# ************************************************************************
# risk premia between govenment and corporate bonds ----
# ************************************************************************
p <- ts_df(
  ts_span(
    ts_c(
      `Notation AA-AAA`  = ts_span(RPAAA1, start_date),
      `Notation BBB-AAA` = ts_span(RPBBB1, start_date)
      ),
    start = start_date
    )
  ) |>
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  geom_vline(xintercept = myLines, colour = "blue", linewidth = 1, alpha = 0.5) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_color_manual(
    breaks = c("Notation AA-AAA", "Notation BBB-AAA"), 
    values = c("#006d64", "#ac004f")
    ) +
  labs(
    title = "Prime de risque à 1 ans (en pp)",
    subtitle = "<span style = 'color: #006d64;'>Entreprises AA-AAA</span>, <span style = 'color: #ac004f;'>Entreprises BBB-AAA</span>", 
    caption = "@econmaett. Source de données: SIX",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())


p <- p + geom_text(x = myLines[1], y = 1.3, label = "Faillite Lehman", colour = "blue", angle = 90, vjust = -1)
p <- p + geom_text(x = myLines[2], y = 1.0, label = "Fin du taux plancher", colour = "blue", angle = 90, vjust = -1)
p <- p + geom_text(x = myLines[3], y = 1.0, label = "Crise Corona", colour = "blue", angle = 90, vjust = -1)

p

ggsave(plot = p, filename = "S01E01_Prime-de-Risque/Fig_Primes-de-Risque.png", width = 8, height = 4)
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
    start = start_date
    )
  ) |> 
  na.omit() |> # Remove the missing observations for the plot
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-1, 1.5), breaks = seq(-1, 1.5, 0.5)) +
  scale_color_manual(
    values = c("#374e8e", "#006d64"), 
    breaks = c("Prime de risque (notation BBB-AAA, en pp)", "Ouverture proc. de faillite (variation, en 1000 ouvertures par an)")
  ) +
  labs(
    title = "Relation avec des ouvertures de procédure de faillite",
    subtitle = "<span style = 'color: #374e8e;'>Prime de risque (BBB-AAA, en pp)</span>, <span style = 'color: #006d64;'>Ouvert. (variat., en 1000 pa)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS)",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E01_Prime-de-Risque/Fig_Procedures-de-Faillite-et-Prime-de-Risque.png", width = 8, height = 4)
graphics.off()
# END