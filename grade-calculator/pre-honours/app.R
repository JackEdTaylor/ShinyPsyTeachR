library(shiny)
library(shinyjs)
library(tidyverse)

course <- read_csv("course.csv")

level_tabs <- lapply(1:length(unique(course$level)), function(i) {
        level_label <- unique(course$level)[i]
        level_data <- dplyr::filter(course, level==level_label)
        
        module_sections <- lapply(1:length(unique(level_data$module)), function(j) {
            module_label <- unique(level_data$module)[j]
            module_data <- dplyr::filter(level_data, module == module_label)
            
            sliders <- lapply(1:nrow(module_data), function(k) {
                assessment_label <- sprintf("%s (%g%%)", module_data$assessment[k], module_data$module_proportion[k]*100)
                assessment_id <- sprintf("ass_%i_%i_%i", i, j, k)
                column(6, sliderInput(assessment_id, assessment_label, 0, 22, 0, 1))
            })
            
            fluidRow(column(12, tags$h4(shiny::HTML(sprintf("<b>%s</b>", module_label)))), sliders)
            
        })
        
        tabPanel(
            level_label,
            tags$br(),
            module_sections
        )
    })

loading_done <- function(null_test) {
    hide(id = "loading_page", anim = TRUE, animType = "slide")
    show("main_content")
}

ui <- fluidPage(
    
    useShinyjs(),
    tags$head(
        tags$link(href = "style.css", rel="stylesheet")
    ),
    div(
        id = "loading_page",
        img(src = "psyteachr_hex.png", class = "center-fit"),
        tags$br(), tags$br(),
        icon("spinner", class="fa-spin")
    ),
    hidden(
        div(
            id = "main_content",
            
            fluidPage(
                
                titlePanel("Pre-Honours Grade Calculator"),
                
                sidebarLayout(
                    sidebarPanel(
                        width = 8,
                        tags$p("This web app can help you calculate your grade, and show how different modules contribute to your grade. It can also be used to see how different marks in different modules will affect your overall grade."),
                        tags$br(),
                        tags$p("Enter grades for each level below, by clicking the desired tab for the level and dragging the sliders, and see the effect on the overall grade."),
                        tags$br(),
                        do.call(tabsetPanel, level_tabs)
                    ),
                    
                    mainPanel(
                        
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    
    loading_done()
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
