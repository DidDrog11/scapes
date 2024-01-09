if (!require("pacman")) install.packages("pacman")

pkgs <- c(
  "googledrive",
  "robotoolbox",
  "crul",
  "glue",
  "RcppSimdJson",
  "httr",
  "dm",
  "DT",
  "here",
  "tidyverse",
  "readxl",
  "viridisLite",
  "geodata",
  "terra",
  "crayon",
  "knitr"
)

pacman::p_load(pkgs, character.only = T)

project_CRS = "EPSG:4326"
utm_nigeria_CRS = "EPSG:26332"

village_state = tibble(abbreviation = c("zug", "dye", "iky", "oki", "oga", "ofo", "eze", "eny", "off"),
                       village = c("Zugu", "Dyegh", "Ikyogbakpev", "Okimbongha", "Ogamanna", "Ofonekom", "Ezeakataka", "Enyandulogu", "Offianka"),
                       state = c("Benue", "Benue", "Benue", "Cross River", "Cross River", "Cross River", "Ebonyi", "Ebonyi", "Ebonyi"))

# Number of states and villages per state
num_states <- 3
villages_per_state <- 3

# Create a vector of distinct states
states <- rep(1:num_states, each = villages_per_state)

# Define the colour palette for states
state_colours <- viridis(num_states)

# Create a function to generate perceptually distinct colours for villages within each state
generate_village_colours <- function(num_colours) {
  village_colours <- viridis(num_colours)
  return(village_colours)
}

# Create a colour table with distinct but related colours for villages within each state
colour_table <- tibble(
  state_colour = rep(state_colours, each = villages_per_state),
  village_colour = generate_village_colours(num_states * villages_per_state)
)

village_state <- tibble(village_state,
                        village_colour = colour_table$village_colour,
                        state_colour = colour_table$state_colour)

write_rds(village_state, here("data", "additional", "village_state.rds"))

village_colours <- setNames(village_state$village_colour, village_state$village)
write_rds(village_colours, here("data", "additional", "village_colours.rds"))
