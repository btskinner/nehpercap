## -----------------------------------------------------------------------------
##
## [ PROJ ] Plot per capita NEH appropriations
## [ FILE ] appropriations.R
## [ AUTH ] Benjamin Skinner
## [ INIT ] 29 April 2025
##
## -----------------------------------------------------------------------------

## libraries
libs <- c("tidyverse", "rvest", "fredr", "ggtext", "emoji")
sapply(libs, require, character.only = TRUE)

## paths
args <- commandArgs(trailingOnly = TRUE)
root <- ifelse(length(args) == 0, file.path(".."), args)
fig_dir <- file.path(root, "figures")
scr_dir <- file.path(root, "scripts")

## -----------------------------------------------------------------------------
## FRED
## -----------------------------------------------------------------------------

## -------------------------------------
## population (1000s)
## -------------------------------------
pop <- fredr(series_id = "B230RC0A052NBEA",
             observation_start = as.Date("1965-01-01"),
             observation_end = as.Date("2025-01-01")) |>
  mutate(year = year(date)) |>
  select(year, pop = value)

## -----------------------------------------------------------------------------
## NEH appropriations
## -----------------------------------------------------------------------------

## -------------------------------------
## scrape
## -------------------------------------

df <- read_html("https://www.neh.gov/neh-appropriations-history") |>
  html_node(".field-body-rte") |>
  html_table() |>
  rename(fy = `Fiscal Year`,
         ap = `Appropriations`)

## -------------------------------------
## wrangle
## -------------------------------------

df <- df |>
  left_join(pop, by = c("fy" = "year")) |>
  mutate(ap = parse_number(ap)) |>
  mutate(ap_pc = ap / (pop * 1000)) |>
  pivot_longer(cols = "ap_pc",
               names_to = "type") |>
  arrange(desc(fy)) |>
  group_by(type) |>
  mutate(rol_value = cumsum(value),
         age = 2025 - fy) |>
  ungroup()

## -----------------------------------------------------------------------------
## get relevant purchase prices
## -----------------------------------------------------------------------------

## one month of netflix (https://help.netflix.com/en/node/24926)
netflix <- 17.99

## dozen large (A) eggs
eggs <- fredr(series_id = "APU0000708111",
              observation_start = as.Date("2025-03-01"),
              observation_end = as.Date("2025-03-01")) |> pull(value)

## gallon of gas
gas <- fredr(series_id = "APU000074714",
              observation_start = as.Date("2025-03-01"),
              observation_end = as.Date("2025-03-01")) |> pull(value)

## show
eggs
netflix
gas * 8
df |> select(age, rol_value) |> print(n = 60)

## ages that are closest for the three values are 13, 36, and 50

## -----------------------------------------------------------------------------
## plot
## -----------------------------------------------------------------------------

g <- df |>
  ggplot(aes(x = age, y = rol_value, color = type, linetype = type)) +
  geom_line(linewidth = 1) +
  geom_point(data = df |> filter(age %in% c(13, 36, 50)),
             aes(x = age, y = rol_value),
             shape = c(emoji("egg"), emoji("popcorn"), emoji("fuelpump")),
             size = 16) +
  geom_text(data = df |> filter(age %in% c(13, 36, 50)),
            aes(label = scales::dollar(rol_value)),
            nudge_x = c(0.75, -0.9, -1),
            nudge_y = c(-0.75, 0.75, 1),
            hjust = c(0, 1, 1),
            vjust = c(1, 0, 0),
            size = 5) +
  scale_x_continuous(breaks = c(seq(10, 59, 10)),
                     minor_breaks = seq(1, 59, 1),
                     expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 30, 5),
                     minor_breaks = seq(0, 30, 1),
                     labels = scales::label_dollar(),
                     expand = c(0,0),
                     limits = c(0,30)) +
  scale_linetype_manual(values = c("solid")) +
  scale_color_manual(values = c("#000000")) +
  ## note
  geom_textbox(aes(x = 1.5, y = 29.5,
                   label = paste(
                     "Between its founding in 1965 and fiscal year 2024,",
                     "the National Endowment for the Humanities received",
                     "congressional appropriations totaling **$7.37 billion**.",
                     "NEH used this funding to support thousands of individuals",
                     "and cultural organizations \u2014 both directly",
                     "through various grant programs and indirectly through",
                     "funds disbursed by the 56 state and jurisdictional",
                     "humanities councils. Beyond these",
                     "appropriations, grantees raised millions of dollars",
                     "more from non-federal donors as part of grant matching",
                     "requirements.",
                     "\n\n",
                     "To put NEH appropriations in perspective,",
                     "this chart shows cumulative annual per capita",
                     "contribution to NEH by age, that is, **the total dollar",
                     "amount the average taxpayer has given to NEH over",
                     "their lifetime**. A few relevant purchases are helpfully",
                     "included to show what a person could have bought instead,",
                     "had they kept their personal NEH contribution for",
                     "themselves..."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.4, "npc")) +
  ## eggs
  geom_textbox(aes(x = 2, y = 12,
                   label = paste(
                     "Just in time to feed a growth spurt, a person at",
                     "13 years old has contributed enough to NEH that",
                     "they could instead purchase",
                     "one dozen eggs (**$6.23/dozen**)."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.2, "npc")) +
  ## Netflix
  geom_textbox(aes(x = 28, y = 12,
                   label = paste(
                     "Millennials who are at least 36 years old could exchange",
                     "their nearly four decades worth of NEH support for a one",
                     "month subscription to Netflix (**$17.99/month**)."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.195, "npc")) +
  ## gas
  geom_textbox(aes(x = 46, y = 21,
                   label = paste(
                     "Those 50 years old or older \u2014 who have been",
                     "contributing to the agency for nearly its entire",
                     "existence \u2014 could instead have saved that money",
                     "to purchase about a half tank of gas (8 gallons",
                     "at **$3.23/gallon**)."
                   )),
               fill = "white",
               orientation = "upright",
               hjust = 0,
               vjust = 1,
               width = unit(.2, "npc")) +
  ## arrows
  geom_segment(data = df |> filter(age == 13),
               aes(x = 8,
                   xend = age - 1.25,
                   y = 8.4,
                   yend = rol_value + 0.5),
               arrow = grid::arrow(type = "closed"),
               linewidth = 0.5) +
  geom_segment(data = df |> filter(age == 36),
               aes(x = age - 3,
                   xend = age - 0.75,
                   y = 12 + 0.25,
                   yend = rol_value - 1.5),
               arrow = grid::arrow(type = "closed"),
               linewidth = 0.5) +
  geom_segment(data = df |> filter(age == 50),
               aes(x = age + 2,
                   xend = age + 0.5,
                   y = 21 + 0.25,
                   yend = rol_value - 1.5),
               arrow = grid::arrow(type = "closed"),
               linewidth = 0.5) +
  ## labels
  labs(x = "Age",
       y = NULL,
       title = "What's my share of NEH appropriations?",
       caption = c(expression(
         italic(paste("Data sources: ",
                      "https://www.neh.gov/neh-appropriations-history; ",
                      "https://help.netflix.com/en/node/24926; ",
                      "FRED (March 2025)"))),
                      "https://github.com/btskinner/nehpercap")
       ) +
  ## theme
  theme_bw(base_family = "Avenir", base_size = 18) +
  theme(legend.position = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.caption = element_text(hjust = c(1,0), size = 9))

## save
ggsave(filename = file.path(fig_dir, "neh_appropriations_pc_cumulative.png"),
       plot = g,
       width = 16,
       height = 9,
       units = "in",
       dpi = "retina",
       bg = "white")

## -----------------------------------------------------------------------------
## end script
################################################################################
