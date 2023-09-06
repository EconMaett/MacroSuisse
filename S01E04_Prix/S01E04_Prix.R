# ************************************************************************
# R Code [S1E04] "Les prix baissent en Suisse - C'est top n'est-ce pas?" ----
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

start_date <- "2000-01-01"
end_date   <- "2020-04-01"
# floor_date(x = today(), unit = "month") - months(1) # Has to be last observation of CPI
chrecdp   <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")

# ideally put these series into a separate file and access them with the source() comand.
calcIndex <- function(series, weights, baseY) {
  # Useful function to calculate weighted mean of indexed series

  series <- ts_index(series, baseY) * 100

  Index <- sapply(X = seq_len(nrow(series)), FUN = function(i) {
    weighted.mean(x = as.matrix(series[i, ]), w = weights, na.rm = TRUE)
  })

  Index <- xts(Index, order.by = as.Date(index(series)))

  return(Index)
}

# ************************************************************************
# Download the data ----
# ************************************************************************
download.file(
  url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12827309/master", 
  destfile = "S01E04_Prix/IPC.xlsx", 
  mode = "wb"
)

# Import the data ----
Date <- seq(from = as.Date("1982-12-01"), to = as.Date(end_date), by = "month")

# Note that some adjustments have been made manually!
Prix <- read_excel(path = "S01E04_Prix/IPC_Manuelx.xlsx", sheet = "Main")
head(Prix) # Code, PosNo, PosType, Level, Position_D, Missing, Weight 

Comp <- read_excel(path = "S01E04_Prix/IPC_Manuelx.xlsx", sheet = "Components")
head(Comp) # Code, PosNo, PosType, Level, Position_D, Missing, Weight 

Prix <- xts(t(Prix[1:3, 8:ncol(Prix)]), order.by = Date)

Type    <- Comp$PosType
Weight  <- as.numeric(Comp$Weight)
Index   <- xts(t(Comp[, 8:ncol(Comp)]), order.by = Date)
Missing <- Comp$Missing

Weight <- Weight[Type == 4]
Index  <- Index[, Type == 4, ]
Weight[is.na(Weight)] <- 0
Missing <- Missing[Type == 4]
Missing[is.na(Missing)] <- 0

Baseline <- ts_span(calcIndex(Index, Weight, "2015-12-01"), "2010-12-01")
Counterf <- ts_span(calcIndex(Index[, Missing == 0], Weight[Missing == 0], "2015-12-01"), "2010-12-01")


# ************************************************************************
# Create charts ----
# ************************************************************************

## IPC officiel vs sans categories imputés ----
p <- ts_df(
  ts_c(
    `IPC (officiel)` = ts_pcy(ts_span(Prix[, 1], "2010-12-01")),
    # `IPC (propre calcul)`     = ts_pcy(Baseline),
    `IPC (sans categories avec prix imputés en Avril 2020)` = ts_pcy(Counterf)
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date("2010-12-01"), date("2020-12-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_color_manual(
    breaks = c("IPC (officiel)", "PC (propre calcul)", "IPC (sans categories avec prix imputés en Avril 2020)"),
    values = c("#374e8e", "#006d64", "#ac004f")
  ) +
  labs(
    title = "Taux d'inflation (par rapport à l'année précédente, en %)",
    subtitle = "<span style = 'color: #006d64;'>IPC (officiel)</span>, <span style = 'color: #ac004f;'>IPC (sans categories avec prix imputés en Avril 2020)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) + 
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E04_Prix/Fig_Prix-Impute.png", width = 8, height = 4)
graphics.off()


## IPC domestique vs importé ----
p <- ts_df(
  ts_c(
    `IPC` = ts_pcy(ts_span(Prix[, 1], "2015-12-01")),
    `Biens domestiques` = ts_pcy(ts_span(Prix[, 2], "2015-12-01")),
    `Biens importés` = ts_pcy(ts_span(Prix[, 3], "2015-12-01"))
    )
  ) |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(limits = c(date("2016-12-01"), date("2020-12-01")), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(limits = c(-2, 2), breaks = seq(-2, 2, 0.5)) +
  scale_color_manual(
    breaks = c("IPC", "Biens domestiques", "Biens importés"),
    values = c("#374e8e", "#006d64", "#ac004f")
  ) +
  labs(
    title = "Taux d'inflation (par rapport à l'année précédente, en %)",
    subtitle = "<span style = 'color: #374e8e;'>IPC</span>, <span style = 'color: #006d64;'>Biens domestiques</span>, <span style = 'color: #ac004f;'>Biens importés</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E04_Prix/Fig_Inflation.png", width = 8, height = 4)
graphics.off()
# END