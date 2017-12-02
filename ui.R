library(shiny)
library(shinythemes)
# Define UI for application that draws a histogram
shinyUI(
  tagList(
    shinythemes::themeSelector(),
    navbarPage(
      theme = "superhero",
      "Title",
      tabPanel("Student Enrollment",
               sidebarPanel(
                 # fileInput("file", "File input:"),
                 # textInput("txt", "Text input:", "general"),
                 selectInput("School",label="See Students in", choices=ent_new$SCHOOL)
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Student Age Component",
                            plotOutput("ageplot"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("comment1")
                   ),
                   tabPanel("Student Entrance Age and Sex",
                            plotOutput("ageplot"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("comment2")
                   ),
                   tabPanel("Drop Out Rate", "This part is still blank")
                 )
               )
      ),
      tabPanel("Students Acadamic Performance", 
               sidebarPanel(
                 numericInput("year_ec", "Input a year", 2004, min = 2002, max = 2004),
                 textInput("Semester",label = "Choose a Semester", value = ""),
                 selectInput("Grd",label="Select a Grade", choices=c(9,10,11,12)),
                 textInput("Subject",label = "Select an Exam", value = "")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Overview of Academic Performance",
                     plotOutput("acadplot"),
                     h4("how the scores calculated and what each bar means")
                   ),
                   tabPanel("Grades of each Exam",
                     plotOutput("subject_plot")
                   ))
               )
              ),
      tabPanel("Teacher", "This panel is to present teacher's qualification and numbers",
               fluidRow(
                 column(6,  selectInput(
                 "School_tch",label="Select a School", choices=staff_ed$SCHOOL)
                 ),
                fluidRow(
                  column(6,  plotOutput()
                  ),
                  column(6,  plotOutput()
                  )
                )
               )
      )
    )
  )
)
