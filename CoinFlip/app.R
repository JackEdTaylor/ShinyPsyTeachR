library(shiny)
library(shinydashboard)
library(tidyverse)
library(magrittr)

sidebarwidth <- 200

# Define UI for application that draws a histogram
ui <- dashboardPage(
    skin = "blue",
    dashboardHeader(
        title = "CoinFlip", titleWidth = sidebarwidth,
        tags$li(a(href = "https://github.com/JackEdTaylor/ShinyPsyTeachR",
                  HTML(paste(icon("github"), "&nbsp;GitHub")),
                  title = "GitHub Repository"), class="dropdown")
    ),
    
    dashboardSidebar(
        width = sidebarwidth,
        numericInput("n_sim", "Number of Simulations", value = 100, min = 1, step =1, width = "100%"),
        numericInput("n_flip", "Number of Flips", value = 10, min = 1, step = 1, width = "100%"),
        sliderInput("p_heads", "Probability of Heads", value = 0.5, min = 0, max = 1, step = 0.05, width = "100%")
    ),
    
    dashboardBody(
        tags$style(type="text/css", ".recalculating {opacity: 1.0;}"),  # prevent plots from "greying out" while loading
        box(
            title = "Results", status = "primary", solidHeader = FALSE, width = 12, collapsible = FALSE,
            plotOutput("dist_plot"),
            uiOutput("animated_slider")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    flips <- reactive({
        n_sim <- input$n_sim
        n_flip <- input$n_flip
        p_heads <- input$p_heads
        
        tibble(
            sim_nr = 1:n_sim,
            n_heads = sapply(1:n_sim, function(sim) sum(base::sample(0:1, n_flip, replace=TRUE, prob=c(1-p_heads, p_heads))))
        )
        
    })
    
    output$animated_slider <- renderUI({
        # mention these so that the slider is re-rendered when they change
        input$n_flip
        input$p_heads
        list(
            # build animated slider
            sliderInput("i_sim", NULL, value = 1, min = 0, max = input$n_sim, step = 1, width = "100%", animate = animationOptions(interval = round(20000/input$n_sim)), pre="Simulation "),
            # play automatically once rendered
            tags$script("$(document).ready(function(){ setTimeout(function() {$('.slider-animate-button').click()},5); });")
        )
    })
    
    output$dist_plot <- renderPlot({
       if (is.null(input$i_sim)) {
           NULL
       } else {
           n_flip <- input$n_flip
           this_sim <- input$i_sim
           
           max_obs <- flips() %>%
               group_by(n_heads) %>%
               summarise(obs = n()) %>%
               pull(obs) %>%
               max()
           
           flips() %>%
               filter(sim_nr <= this_sim) %>%
               mutate(is_this_sim = factor(ifelse(sim_nr == this_sim, "This Simulation", "Previous Simulations"), levels = c("This Simulation", "Previous Simulations"))) %>%
               ggplot(aes(x = n_heads, fill = is_this_sim)) +
               geom_bar() +
               labs(
                   x = sprintf("Number of Heads out of %i Flips", n_flip),
                   y = "Number of Simulations Observed"
               ) +
               theme_minimal() +
               theme(legend.position = "top", legend.title = element_blank()) +
               scale_x_continuous(limits = c(0, n_flip), breaks = scales::pretty_breaks()) +
               scale_y_continuous(limits = c(0, max_obs)) +
               scale_fill_manual(values=c("#3c8dbc", "#999999"))
       }
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
