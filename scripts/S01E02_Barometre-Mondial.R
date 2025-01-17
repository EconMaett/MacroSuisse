# ************************************************************************
# R Code [S1E02] "Les saints de glace jettent un froid sur la Suisse" ----
# ************************************************************************
# Video available on www.youtube.com/channel/UCJpACBsnn1eQTObWz5LniGg
# Feel free to copy, adapt, and use this code for your own purposes.
# Matthias Spichiger (matthias.spichiger@bluewin.ch)
# ************************************************************************
library(tsbox)
library(tidyverse)
library(forecast)
library(xts)
library(readxl)
library(ggtext)

source("R/getForecastVariance.R")

chrecdp    <- read_csv(file = "Recession-Dates/Recession-Dates_CEPR_EA_Monthly_Midpoint.csv")
start_date <- "1999-01-01"
# Last date should be first date of last quarter
end_date   <- round_date(x = today(), unit = "quarter") - months(3)


# ************************************************************************
# Download the data ----
# ************************************************************************
# Source: https://kof.ethz.ch/fr/previsions-indicateurs/indicateurs/kof-globalbaro.html
urls <- c(
  "https://datenservice.kof.ethz.ch/api/v1/public/ts?keys=ch.kof.globalbaro.coincident,ch.kof.globalbaro.leading,ch.kof.globalbaro.gdp_reference&mime=xlsx&name=date,globalbaro_coincident,globalbaro_leading,gdp_reference",
  "https://www.seco.admin.ch/dam/seco/de/dokumente/Wirtschaft/Wirtschaftslage/VIP%20Quartalssch%C3%A4tzungen/qna_p_csa.xlsx.download.xlsx/qna_p_csa.xlsx",
  "https://www.six-group.com/exchanges/downloads/indexdata/h_vsmi_30.csv"
)

names <- c("Barometre-Mondial.xlsx", "PIB-Suisse.xlsx", "VSMI.csv")

# Ideally a tryCatch() statement
for (i in seq_along(urls)) {
  download.file(url = urls[i], destfile = paste0("S01E02_Barometre-Mondial/", names[i]), mode = "wb", quiet = FALSE, method = "curl")
}

# ************************************************************************
# Data preparation ----
# ************************************************************************

## GDP ----
# Quarterly real GDP, adjusted for seasonal and holiday effects and sport events
PIB <- read_excel(
  path = "S01E02_Barometre-Mondial/PIB-Suisse.xlsx",
  sheet = "real_q",
  range = cell_limits(ul = c(11, 1), lr = c(NA, NA)) # Specify open rectangle, upper left = A11
)

# Date: Year, first month of quarter, first day
PIB <- PIB |> 
  mutate(Date = as.Date(paste0(`...1`, "-", `...2`*3 - 2, "-01"))) |> 
  rename(PIB = `...3`) |> 
  select(Date, PIB)

# Create an xts object
PIB <- xts(x = as.numeric(PIB$PIB), order.by = PIB$Date)
# Note: Because PIB shows a clear upwards trend, we want to take quarter-on-quarter growth rates,
# Note that if we regress non-stationary series on each other, we end up with spurious regression results.

## Baro ----
# Leading KOF global barometer
Baro <- read_excel(path = "S01E02_Barometre-Mondial/Barometre-Mondial.xlsx", sheet = "Sheet1")

Baro <- Baro |> 
  mutate(Date = as.Date(paste0(date, "-01"))) |> 
  select(Date, globalbaro_leading)

# Create time series (use aggregate GDP and the Barom leading indicator)
Baro <- xts(x = as.numeric(Baro$globalbaro_leading), order.by = Baro$Date)
# Note that such survey indices are typically designed to be stationary.

# Normalize the barometer and then scale it to the GDP growth rates
# so that the mean and standard deviation are the same for both series. 
mPIB   <- as.numeric(mean(ts_pc(PIB), na.rm = TRUE))
sdPIB  <- as.numeric(sqrt(var(ts_pc(PIB), na.rm = TRUE)))

mBaro  <- as.numeric(mean(Baro, na.rm = TRUE))
sdBaro <- as.numeric(sqrt(var(Baro, na.rm = TRUE)))

# Baro' = {(Baro - E[Baro])/SD[Baro]}*SD[PIB] + E[PIB]
Baro   <- ((Baro - mBaro) / sdBaro) * sdPIB + mPIB


## Volatility ----
# VSMI index is a measure of the volatility of the Swiss Market Index SMI
Vol <- read_delim(file = "S01E02_Barometre-Mondial/VSMI.csv", delim = ";")
head(Vol) # Date [DD.MM.YYYY], ISN, Indexvalue

Vol <- Vol |> 
  mutate(Date = dmy(Date)) |> 
  select(Date, Indexvalue)

# Create a time series object
Vol <- xts(x = as.numeric(Vol$Indexvalue), order.by = Vol$Date)

# We aggregate the daily data to quarterly frequency,
# by aggregating over the mean
Vol <- ts_frequency(Vol, to = "quarter", aggregate = "mean", na.rm = TRUE)
Vol <- ts_span(Vol, start_date)

# Normalize the VSMI in the same way as the barometer
mVol    <- as.numeric(mean(Vol, na.rm = TRUE))
sdVol   <- as.numeric(sqrt(var(Vol, na.rm = TRUE)))

# VSMI' = {(VSMI - E[VSMI])/SD[VSMI]}*SD[PIB] + E[PIB]
VolNorm <- (Vol - mVol) / sdVol * sdPIB + mPIB

# Now that we have normalized both the barometer and the SMI volatility to have the
# same mean and standard deviation as the quarter-on-quarter growth rates of GDP (in %),
# we can see how the series move together.
# There is a negative relationship between stock market volatility and GDP growth rates
# and a positive relationship between the leading barometer and GDP growth rates.

# To compute the correlation between the leading barometer and GDP growth rates,
# we need to change the barometer to a quarterly frequency by aggregating over the mean.
BaroQ   <- ts_frequency(Baro, to = "quarter", aggregate = "mean", na.rm = TRUE)
# Combine the quarter-on-quarter GDP percentage changes, the quarterly barometer, 
# and its lag
myDataQ <- data.frame(ts_span(ts_c(ts_pc(PIB), BaroQ, lag(BaroQ, 1)), start_date))
colnames(myDataQ) <- c("PIB", "Baro", "Baro.l")
head(myDataQ)
print(cor(myDataQ[1:dim(myDataQ)[1] - 1, ]))

BaroQ.l <- lag(BaroQ, 1)

# ************************************************************************
# Create charts -----
# ************************************************************************

## PIB vs Baro ----

# PIB has one value for the first day of each quarter, and Baro has one for the first day of each month,
# To make implicit missings visible as NAs, convert xts to ts objects.
# Before plotting, remove the NAs with na.omit().
p <- ts_df(
  ts_span(
    ts_c(
      `Croissance du PIB Suisse (en %)` = ts_pc(ts_ts(PIB)),
      `Baromètre mondial (index avancé normalisé)` = ts_ts(Baro)
      ),
    start = start_date
    )
  ) |> 
  na.omit() |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = seq(-8, 8, 4)) +
  scale_color_manual(
    breaks = c("Croissance du PIB Suisse (en %)", "Baromètre mondial (index avancé normalisé)"), 
    values = c("#374e8e", "#006d64")
    ) +
  labs(
    title = "Activité économique Suisse et barométre mondial",
    subtitle = "<span style = 'color: #374e8e;'>Croissance du PIB Suisse (en %)</span>, <span style = 'color: #006d64;'>Baromètre mondial (index avancé normalisé)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) + 
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E02_Barometre-Mondial/Fig_PIB-et-Barometre-Mondial.png", width = 8, height = 4)
graphics.off()

## BIP vs Baro & VolNorm ----
p <- ts_df(
  ts_span(
    ts_c(
      `Croissance du PIB (en %)` = ts_pc(ts_ts(PIB)),
      `Baromètre mondial (index av. norm.)` = ts_ts(Baro),
      `Incertitude marchés financiers (norm.)` = ts_ts(VolNorm)
      ),
    start = start_date
    )
  ) |> 
  na.omit() |> 
  ggplot() +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = recession_start, xmax = recession_end, ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  geom_line(mapping = aes(x = time, y = value, color = id), linewidth = 1) +
  scale_x_date(expand = c(0, 0), limits = c(date(start_date), today()), date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = seq(-8, 8, 4)) +
  scale_color_manual(
    values = c("#374e8e", "#006d64", "#ac004f"), 
    breaks = c("Croissance du PIB (en %)", "Baromètre mondial (index av. norm.)", "Incertitude marchés financiers (norm.)")
    ) +
  labs(
    title = "Données modèle de prévision",
    subtitle = "<span style = 'color: #374e8e;'>Croissance du PIB (en %)</span>, <span style = 'color: #006d64;'>Baromètre mondial (index av. norm.)</span>, <span style = 'color: #ac004f;'>Incertitude marchés financiers (norm.)</span>",
    caption = "@econmaett. Source de données: Office fédéral de la statistique (OFS), SIX.",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E02_Barometre-Mondial/Fig_PIB-Barometre-Volatilite.png", width = 8, height = 4)
graphics.off()


# ************************************************************************
# Estimate simple forecasting model for computing the probability of a negative growth rate ----
# ************************************************************************
# PIB_t = alpha + beta * BAroQnorm_t + gamma * VSMIQnorm_t + epsilon_t

XVars <- ts_span(ts_ts(ts_c(BaroQ, Vol)), start_date, end_date)
YVars <- ts_span(ts_ts(ts_pc(PIB)), start_date, end_date)

Model <- Arima(
  y = YVars, 
  order = c(0, 0, 0), 
  include.constant = TRUE, 
  xreg = XVars[1:(nrow(XVars) - 1), ] # We have two more quarters of input data than we have GDP growth rate observations
  )

# Use the model to generate forecasts
Forecast <- forecast(
  object = Model, 
  xreg = (XVars[(nrow(XVars) - 1):nrow(XVars), ]), 
  h = 2, 
  level = c(50, 80, 90, 95)
  )

p <- autoplot(Forecast, fan = TRUE) +
  geom_hline(yintercept = 0, color = "black", linetype = "solid", show.legend = FALSE) +
  geom_rect(data = chrecdp, aes(xmin = decimal_date(recession_start), xmax = decimal_date(recession_end), ymin = -Inf, ymax = +Inf), fill = "darkgrey", alpha = 0.3) +
  
  scale_x_continuous(limits = c(2000, 2025)) +
  scale_y_continuous(expand = c(0, 0), limits = c(-8, 8), breaks = seq(-8, 8, 4)) +
  labs(
    title = "Prévision croissance PIB Suisse (en %)",
    subtitle = "",
    caption = "",
    x = "", y = ""
  ) +
  theme_bw() +
  theme(plot.subtitle = element_markdown(), legend.position = "none") +
  theme(panel.grid.minor = element_blank())

p

ggsave(plot = p, filename = "S01E02_Barometre-Mondial/Fig_PIB-Prevision.png", width = 8, height = 4)
graphics.off()


# # ************************************************************************
# Simulate the forecast density ----
# ************************************************************************

# Simulate the forecast density assuming that the forecast error is normally distributed
# This implies that y(t+h) ~ N(y(t+h|t), sigh^2), that is, the future value of GDP growth
# is normally distributed with a mean equal to the point forecast and variance equal to the
# forecast error variance.
# Compute forecast error Variance
sigh2 <- getForecastVariance(Forecast)

NSim    <- 5000 # Note that you can test your codes with a small number of simulations to increase speed and calculate the final results with a higher number
fcsth   <- Forecast$mean
SimFcst <- matrix(NA, nrow = 2, ncol = NSim)
H       <- length(Forecast$mean)

# Simulate NSim-times a normal distribution with the 1-step and 2-step ahead
# forecast errors
SimFcst[1, ] <- rnorm(n = NSim, mean = fcsth[H - 1], sd = sqrt(sigh2[H - 1]))
SimFcst[2, ] <- rnorm(n = NSim, mean = fcsth[H], sd = sqrt(sigh2[H]))
SimFcst      <- xts((SimFcst), order.by = as.Date(c("2023-10-01", "2024-01-01")))

# Compute the probability of a negative growth rate
PNeg2023Q1 <- mean(SimFcst[1, ] < 0)
PNeg2023Q2 <- mean(SimFcst[2, ] < 0)

# A nice table would be useful here, together with the point estimate
print(c(PNeg2023Q1, PNeg2023Q2))

# END