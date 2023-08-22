# ************************************************************************
# R Code [S1E04] "Les prix baissent en Suisse - C'est top n'est-ce pas?" ----
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
library(tsbox)
library(xts)
library(readxl)
library(ggtext)

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

startDate <- "2000-01-01"
endDate   <- "2020-04-01"
# floor_date(x = today(), unit = "month") - months(1) # Has to be last observation of CPI

# ************************************************************************
# Download the data ----
# ************************************************************************
download.file(
  url = "https://www.bfs.admin.ch/bfsstatic/dam/assets/12827309/master", 
  destfile = "S01E04_Prix/IPC.xlsx", 
  mode = "wb"
)

# Import the data ----
Date <- seq(from = as.Date("1982-12-01"), to = as.Date(endDate), by = "month")

# Note that some adjustments have been made manually!
Prix <- read_excel(path = "S01E04_Prix/IPC_Manuelx.xlsx", sheet = "Main")
head(Prix) # Code, PosNo, PosType, Level, Position_D, Missing, Weight 

Comp <- read_excel(path = "S01E04_Prix/IPC_Manuelx.xlsx", sheet = "Components")
head(Comp) # Code, PosNo, PosType, Level, Position_D, Missing, Weight 

Prix <- xts(t(Prix[1:3, 8:ncol(Prix)]), order.by = Date)
plot(Prix)
graphics.off()

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

plot(ts_c(Baseline, Counterf, Prix[, 1]))
graphics.off()

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
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(
    title = "Taux d'inflation (par rapport à l'année précédente, en %)",
    subtitle = "<span style = 'color: #1B9E77;'>IPC (officiel)</span>, <span style = 'color: #D95F02;'>IPC (sans categories avec prix imputés en Avril 2020)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) + 
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) + 
  guides(col = guide_legend(nrow = 1, byrow = TRUE)) + 
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) + 
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) + 
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E04_Prix/PrixImpute.png", width = 8, height = 4)
graphics.off()


## IPC domestique vs importé ----
p <- ts_df(
  ts_c(
    `IPC` = ts_pcy(ts_span(Prix[, 1], "2015-12-01")),
    `Biens domestiques` = ts_pcy(ts_span(Prix[, 2], "2015-12-01")),
    `Biens importés` = ts_pcy(ts_span(Prix[, 3], "2015-12-01"))
    )
  ) |> 
  ggplot(mapping = aes(x = time, y = value, color = id)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed", show.legend = FALSE) +
  scale_x_date(date_breaks = "1 year", date_labels = "%y") +
  labs(
    title = "Taux d'inflation (par rapport à l'année précédente, en %)",
    subtitle = "<span style = 'color: #1B9E77;'>Biens domestiques</span>, <span style = 'color: #D95F02;'>Biens importés</span>, <span style = 'color: black;'>IPC</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) +
  theme_minimal() + 
  scale_color_brewer(palette = "Dark2") +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "black", "#E7298A", "#E6AB02", "black", "#A6761D")) +
  theme(legend.position = "bottom", legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-20, -5, 0, -5)) + 
  guides(col = guide_legend(nrow = 1, byrow = TRUE)) + 
  theme(legend.title = element_blank()) +
  theme(axis.line = element_line(colour = "black", linewidth = 0.1)) + 
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA)) + 
  theme(text = element_text(family = "Palatino")) +
  theme(panel.grid.major = element_line(colour = "black", linewidth = 0.1, linetype = "dotted"), panel.grid.minor = element_blank()) +
  theme(plot.subtitle = element_markdown(), legend.position = "none")

p

ggsave(filename = "S01E04_Prix/Inflation.png", width = 8, height = 4)
graphics.off()
# END