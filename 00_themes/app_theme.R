# App Theme

library(bslib)
library(shiny)

# SPECIALIZED SPECS
# THEME PARAMETERS ----
# LOGO to be established
LOGO         <- "dashloop_dark_logo.png"
TITLE        <- HTML("Data Science Salaries")
FIRST_TAB    <- "Intro"
SECOND_TAB <- "Explore"
THIRD_TAB   <- "Regression"

# BUSINESS SCIENCE THEME ATTRIBUTES ----
FONT_HEADING <- "Oswald"
FONT_BASE    <- "Courier Prime"
# c("#000000cc", "#FF7F00", "#2C3E50")
PRIMARY      <- "#000000cc"
SUCCESS      <- "#2C3E50"
INFO         <- "#A6CEE3"
WARNING      <- "#FF9A49"
DANGER       <- "#FF6647"
FG           <- PRIMARY
BG           <- "#FFFFFF"

app_theme_base <- bs_theme(
  font_scale   = 1.0,
  heading_font = font_google(FONT_HEADING, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
  base_font    = font_google(FONT_BASE, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
  primary      = PRIMARY,
  success      = SUCCESS,
  info         = INFO, 
  warning      = WARNING, 
  danger       = DANGER,
  fg           = FG,
  bg           = BG,
  "navbar-bg"  = PRIMARY,
  "body-color" = PRIMARY, 
  "accordion-button-active-bg"    = "white",
  "accordion-button-active-color" = PRIMARY,
  "bs-accordion-color" = "white",
  "light" = BG
)



# CYBERPUNK THEME ATTRIBUTES ----
FONT_HEADING <- "Permanent Marker"
FONT_BASE    <- "Roboto"
PRIMARY      <- "#333333"
SUCCESS      <- "#FF6647"
INFO         <- "#FF6647"
WARNING      <- "#FF6647"
DANGER       <- "#2C3E50"
FG           <- "#FF6647"
BG           <- "white"


app_theme_cyberpunk <- bs_theme(
  font_scale   = 1.0,
  heading_font = font_google(FONT_HEADING, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
  base_font    = font_google(FONT_BASE, wght = c(300, 400, 500, 600, 700, 800), ital = c(0, 1)),
  primary      = PRIMARY,
  success      = SUCCESS,
  info         = INFO, 
  warning      = WARNING, 
  danger       = DANGER,
  fg           = FG,
  bg           = BG,
  "navbar-bg"  = "#FF6647",
  "body-color" = "#2C3E50", 
  "accordion-button-active-bg"    = "#2C3E50",
  "accordion-button-active-color" = "#FF6647",
  "bs-accordion-color" = "white",
  "light" = BG
)

specialized_title <- list(
  tags$img(
    src   = LOGO,
    id    = "logo",
    style = "height:46px;margin-right:24px;-webkit-filter: drop-shadow(5px 5px 5px #222);"
  ),
  h4(TITLE, id = "title")
)