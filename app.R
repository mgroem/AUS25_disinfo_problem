
rm(list=ls())
options(scipen=999)


# Load/install packages
required_packages <- c("shiny", "ggplot2", "dplyr", "ggthemes", "plotly", "tidyr")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)

library(shiny)
library(ggplot2)
library(dplyr)
library(ggthemes)
library(plotly)
library(tidyr)

# Load your data
df_counts <- read.csv("df_counts.csv")

# UI
ui <- fluidPage(
  plotlyOutput("threatPlot")
)

# Define server
server <- function(input, output, session) {
  
  output$threatPlot <- renderPlotly({
    
    # Responsive dimensions
    screen_width <- session$clientData$output_threatPlot_width
    width <- screen_width
    height <- if (!is.null(width)) round(width * 0.75) else 600  # maintain 4:3 aspect ratio
    
    # Add response labels
    df_counts <- df_counts %>%
      mutate(response_label = case_when(
        value == 1 ~ "No problem at all",
        value == 2 ~ "A minor problem",
        value == 3 ~ "A moderate problem",
        value == 4 ~ "A big problem",
        value == 5 ~ "A very big problem",
        value == 6 ~ "Don't know",
        TRUE ~ as.character(value)
      ))
    
    # Create plot
    p <- ggplot(df_counts, aes(
      x = factor(value),
      y = pct,
      fill = variable,
      text = paste0(response_label, ": ", sprintf("%.1f%%", pct))
    )) +
      geom_col(color = "white", width = 0.8) +
      geom_text(aes(y = pct + 2, label = sprintf("%.1f%%", pct)), size = 3, color = "black") +
      scale_x_discrete(
        labels = c(
          "1" = "No problem\nat all",
          "2" = "A minor\nproblem",
          "3" = "A moderate\nproblem",
          "4" = "A big\nproblem",
          "5" = "A very\nbig problem",
          "6" = "Don't\nknow"
        )
      ) +
      scale_fill_economist() +
      labs(
        y = "Percent", x = NULL,
        title = "Perceived threat from mis-/disinformation"
      ) +
      coord_cartesian(ylim = c(0, 40)) +
      theme_economist() +
      theme(
        legend.position = "none",
        panel.spacing = unit(2, "lines"),
        strip.text = element_text(margin = margin(b = 10))
      )
    
    # Convert to plotly
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "closest",
        autosize = FALSE,
        height = height,
        width = width
      )
  })
}

# Run the app
shinyApp(ui, server)


# rsconnect::setAccountInfo(
#  name = "yourname",
#  token = "XXXXXX",
#  secret = "YYYYYY")

# rsconnect::deployApp(".")

