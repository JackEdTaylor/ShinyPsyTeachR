library(shiny)
library(shinyjs)
library(shinydashboard)
library(readr)
library(dplyr)
library(ggplot2)

sidebarwidth <- 800

grade_boundaries <- tribble(
    ~label, ~grade,
    "A1", 22,
    "A2", 21,
    "A3", 20,
    "A4", 19,
    "A5", 18,
    "B1", 17,
    "B2", 16,
    "B3", 15,
    "C1", 14,
    "C2", 13,
    "C3", 12,
    "D1", 11,
    "D2", 10,
    "D3", 9,
    "E1", 8,
    "E2", 7,
    "E3", 6,
    "F1", 5,
    "F2", 4,
    "F3", 3,
    "G1", 2,
    "G2", 1,
    "H", 0
)

round_grade = function(x, dp=0) {
    posneg = sign(x)
    z = abs(x)*10^dp
    z = z + 0.5
    z = trunc(z)
    z = z/10^dp
    z*posneg
}

label_fun <- function(x) {
    tibble(y = sum(x)) %>%
        mutate(y_round = round_grade(y)) %>%
        left_join(grade_boundaries, by=c("y_round" = "grade")) %>%
        select(y, label)
}

course <- read_csv("course.csv") %>%
    mutate(assessment_id = sprintf("ass_%i", row_number()))

level_tabs <- lapply(1:length(unique(course$level)), function(i) {
    level_label <- unique(course$level)[i]
    level_data <- filter(course, level==level_label)
    
    module_sections <- lapply(1:length(unique(level_data$module)), function(j) {
        module_label <- unique(level_data$module)[j]
        module_data <- filter(level_data, module == module_label)
        
        sliders <- lapply(1:nrow(module_data), function(k) {
            assessment_label <- sprintf("%s (%g%%)", module_data$assessment[k], module_data$module_proportion[k]*100)
            assessment_id <- module_data$assessment_id[k]
            column(3, sliderInput(assessment_id, assessment_label, 0, 22, 0, 1))
        })
        
        fluidRow(column(12, tags$h4(shiny::HTML(sprintf("<b>%s</b>", module_label)))), fluidRow(sliders))
        
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

ui <- tagList(
    
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
            
            
            dashboardPage(
                
                
                
                skin = "blue",
                
                dashboardHeader(title = "Pre-Honours Grade Calculator", titleWidth = sidebarwidth,
                                tags$li(a(href = "https://github.com/JackEdTaylor/ShinyPsyTeachR",
                                          HTML(paste(icon("github"), "&nbsp;ShinyPsyTeachR")),
                                          title = "ShinyPsyTeachR GitHub"),
                                        class="dropdown")),
                
                dashboardSidebar(
                    width = sidebarwidth,
                    sidebarMenu(
                        fluidRow(
                            column(12,
                                   shiny::HTML(
                                       'This web app can help you calculate your grade, and show how different assessments contribute to your grade.<br><br>Enter grades for each level below, by clicking the desired tab for the level and dragging the sliders, and see the effect on the overall grade.<br><br>'
                                   )
                            ),
                            column(12, do.call(tabsetPanel, append(level_tabs, c(id="level_tab"))))
                        )
                    )
                ),
                
                dashboardBody(
                    
                    tags$head(
                        tags$link(href="style.css", rel="stylesheet"),
                        tags$link(rel="shortcut icon", href="https://www.gla.ac.uk/favicon.ico")
                    ),
                    
                    uiOutput("results_summary"),
                    uiOutput("results_breakdown")
                )
            )
            
        )
    )
)



server <- function(input, output) {
    
    loading_done()
    
    course_react <- reactive({
        course %>%
            mutate(grade = sapply(course$assessment_id, function(id) input[[id]])) %>%
            mutate(grade_proportion = grade * module_proportion) %>%
            filter(level == input$level_tab)
    })
    
    modules_grades <- reactive({
        course_react() %>%
            group_by(module) %>%
            summarise(grade_module = sum(grade_proportion), module_short = first(module_short))
    })
    
    output$results_summary <- renderUI({
        box(
            title = sprintf("%s Summary", input$level_tab),
            width = 12, status="info", solidHeader = TRUE,
            fluidRow(
                column(12, shiny::HTML(sprintf("<h3>Overall %s Average: <b>%s</b> (%g/22)</h3>", input$level_tab, grade_boundaries$label[grade_boundaries$grade==round_grade(mean(modules_grades()$grade_module))], round_grade(mean(modules_grades()$grade_module)) )))
            )
        )
    })
    
    output$breakdown_plot <- renderPlot({
        
        course_react() %>%
            group_by(module) %>%
            mutate(grade_module = round_grade(sum(grade_proportion))) %>%
            ungroup() %>%
            left_join(grade_boundaries, by=c("grade_module" = "grade")) %>%
            ggplot(aes(x = module_short, y=grade_proportion, fill = assessment_type)) +
            geom_col() +
            stat_summary(aes(x = module_short, y=grade_proportion), fun.data = label_fun, geom = "label", inherit.aes=FALSE, fontface="bold", alpha=0.5) +
            ylim(c(0, 23)) +
            labs(x = "Module", y = "Grade") +
            scale_fill_brewer(palette="Dark2") +
            theme_minimal() +
            theme(
                legend.title = element_blank(),
                legend.position = "bottom"
            )
    })
    
    output$results_breakdown <- renderUI({
        
        box(
            title = sprintf("%s Breakdown", input$level_tab),
            width = 12, status="info", solidHeader = TRUE,
            fluidRow(
                lapply(1:nrow(modules_grades()), function(m) {
                    column(12, shiny::HTML(sprintf("<h4>%s: <b>%s</b> (%g/22)</h4>", modules_grades()$module[m], grade_boundaries$label[grade_boundaries$grade==round_grade(modules_grades()$grade_module[m])], round_grade(modules_grades()$grade_module[m]))))
                }),
                column(12, plotOutput("breakdown_plot", height = 200))
            )
        )
    })
    
}

shinyApp(ui = ui, server = server)
