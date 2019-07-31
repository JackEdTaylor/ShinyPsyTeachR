library(shiny)
library(shinyjs)
library(tidyverse)

course <- read_csv("course.csv")

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
                        tags$head(tags$style(type='text/css', ".nav-tabs {font-size: 19px} ")),
                        tabsetPanel(
                            tabPanel(
                                "Level 1",
                                tags$br(),
                                tags$h4("1A: Biological and Experimental"),
                                fluidRow(
                                    column(6, sliderInput("mark_1_1a_lab1_act", "Lab 1 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab1_mcq", "Lab 1 MCQ", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab2_act", "Lab 2 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab2_mcq", "Lab 2 MCQ", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab3_act", "Lab 3 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab3_mcq", "Lab 3 MCQ", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_lab_presentation", "Group Presentation", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_exam", "Exam", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1a_essay", "Class Essay", 0, 22, 0, 1))
                                ),
                                tags$h4("1B: Social, Developmental, Health & Individual Differences"),
                                fluidRow(
                                    column(6, sliderInput("mark_1_1b_lab1", "Lab 1 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_lab2", "Lab 2 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_lab3", "Lab 3 Activity", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_lab_project", "Group Project", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_lab_reflection", "Portfollio Reflection", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_exam", "Exam", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_1_1b_essay", "Class Essay", 0, 22, 0, 1))
                                )
                            ),
                            tabPanel(
                                "Level 2",
                                tags$br(),
                                tags$h4("2A"),
                                fluidRow(
                                    column(6, sliderInput("mark_2_2a_perception_exam", "Cognitive, Psychobiology, Perception & Visual Cognition Exam", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_2_2a_stats1_exam", "Statistics and Research Methods I Exam", 0, 22, 0, 1))
                                ),
                                tags$h4("2B"),
                                fluidRow(
                                    column(6, sliderInput("mark_2_2b_social_exam", "Social, Individual Differences, Developmental Exam", 0, 22, 0, 1)),
                                    column(6, sliderInput("mark_2_2b_stats2_exam", "Statistics and Research Methods II Exam", 0, 22, 0, 1))
                                )
                            )
                        )
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
