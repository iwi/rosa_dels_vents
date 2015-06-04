# Wind roses experiments
# Load the packages
library(dplyr)
library(circular)
library(ggplot2)

hores.a.radians <- function(hores) {
  radians <- hores * 2 * pi / 24
}

# Wind rose
wind.rose.1 <- function(temps_df, fill_colour) {
  temps_df %>%
    ggplot(aes(x = x)) +
    geom_histogram(width = 1,
             binwidth = 1,
             fill = fill_colour,
             colour = "white") +
    coord_polar() +
    theme_minimal() +
    scale_y_continuous() +
    scale_x_continuous(limits = c(0, 24),
                       breaks = seq(from = 0,
                                    to = 24,
                                    by = 0.5)) ->
    plot
  plot
}

wind.rose.2 <- function(dades, fill_colour) {
  dades %>%
    ggplot(aes(x = x)) +
    geom_histogram(breaks = seq(0, 24),
                   width = 1,
                   fill = fill_colour,
                   colour = "lightgrey") +
    coord_polar(start = 0) +
    theme_minimal() +
    scale_fill_brewer() +
    ylab("Count") +
    ggtitle("Events by Time of day") +
    scale_x_continuous("",
                       limits = c(0, 24),
                       breaks = seq(0, 24),
                       labels = seq(0, 24)) ->
    plot
  plot
}

wind.rose.3 <- function(dades, fill_colour) {
  dades %>%
    ggplot(aes(x = x)) +
    geom_dotplot(binwidth = 1) +
    coord_polar(start = 0) +
    theme_minimal() +
    ylab("Count") +
    ggtitle("Events by Time of day") +
    scale_x_continuous("",
                       limits = c(0, 24),
                       breaks = seq(0, 24),
                       labels = seq(0, 24)) ->
    plot
  plot
}

wind.rose.4 <- function(dades, fill_colour) {
  dades %>%
    ggplot(aes(x = x)) +
    geom_histogram(breaks = seq(0, 24),
                   width = 1,
                   fill = fill_colour,
                   colour = "white") +
    coord_polar(start = 0) +
    theme_minimal() +
    ylab("Count") +
    ggtitle("Events by Time of day") +
    scale_y_continuous(expand = c(0, 50),
                       # limits = c(0, 40),
                       breaks = seq(0, 150, 20)) +
    scale_x_continuous("",
                       limits = c(0, 24),
                       breaks = seq(0, 24),
                       labels = seq(0, 24)) ->
    plot
  plot
}

wind.rose.5 <- function(dades, day_colour, night_colour) {
  dades %>%
    ggplot(aes(x = x, fill = sleep_time)) +
    geom_histogram(breaks = seq(0, 24),
                   width = 1,
                   colour = "white") +
    coord_polar(start = 0) +
#    scale_fill_brewer("", palette = "Set1") +
    scale_fill_manual("", values = c(day_colour, night_colour)) +
    theme_minimal() +
    ggtitle("Events by Time of day") +
    scale_y_continuous(name = "Count",
                       expand = c(0, 50),
                       #limits = c(0, 40),
                       breaks = seq(0, 150, 20)) +
    scale_x_continuous("",
                       limits = c(0, 24),
                       breaks = seq(0, 24),
                       labels = seq(0, 24)) ->
    plot
  plot
}

wind.rose.5(dades = df_temps,
            day_colour = "orange",
            night_colour = "#9999CC")


## generate data
set.seed(1)
vm1 <- rvonmises(n = 1000,
                 mu = circular(hores.a.radians(12.5)),
                 kappa = 3)
vm2 <- rvonmises(n = 1000,
                 mu = circular(hores.a.radians(20)),
                 kappa = 3)

temps1 <- 24 * vm1 / (2 * pi)
temps2 <- 24 * vm2 / (2 * pi)

df_temps1 <- as.data.frame.circular(temps1)
df_temps2 <- as.data.frame(temps2)

df_temps <- rbind(df_temps1, df_temps2)
df_temps$sleep_time <- as.factor(ifelse(df_temps$x > 20 | df_temps$x < 7, "Night", "Day"))

wind.rose.5(df_temps)
wind.rose.1(df_temps, "#9999CC")
wind.rose.2(df_temps, "#9999CC")
wind.rose.4(df_temps, "#9999CC")
