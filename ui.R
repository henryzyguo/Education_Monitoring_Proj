library(shiny)
library(shinythemes)
library(shiny)
library(ggplot2)
library(plotly)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

enr_age <- readRDS("enr_age.RData")
staff_ed <- readRDS("staff_ed.RData")
ent_new <-readRDS("ent_new.RData")
grd9 <- readRDS("grd9.RData")
grd12 <- readRDS("grd12.RData")
staffed.expanded <- readRDS("staffed.expanded.RData")
acad_overview <- readRDS("aca_overview.RData")

# Define UI for application that draws a histogram
shinyUI(
  tagList(
    navbarPage(
      theme = shinytheme("sandstone"),
      "Education Monitoring",
      
      # student enrollment
      tabPanel("Student Enrollment",
               sidebarPanel(
                 # fileInput("file", "File input:"),
                 # textInput("txt", "Text input:", "general"),
                 selectizeInput("School",label="Select Students in School", choices=levels(ent_new$SCHOOL)),
                 selectInput("Grade",label="Students in Grade", choices=c(1:8))
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Student Age Component",
                            h4(""),
                            plotlyOutput("ageplot"),
                            verbatimTextOutput("enr_comment")
                   ),
                   tabPanel("Student Entrance Age and Sex",
                    
                            # adidesta
                            fluidRow(
                              column(6,  plotlyOutput("new_ageplot")
                              ),
                              column(6,  plotlyOutput("new_sexplot")
                              ),
                              column(6, verbatimTextOutput("ent_comment1")
                              ),
                              column(6, verbatimTextOutput("ent_comment2")
                              )
                            )
                   )
                 )
               )
      ),
      # academics
      tabPanel("Students Acadamic Performance", 
                 tabsetPanel(
                   tabPanel("Overview of Academic Performance",
                     plotlyOutput("acadplot"),
                     verbatimTextOutput("acad_overview")
                   ),
                   tabPanel("Grades of each Exam",
                            sidebarPanel(
                              numericInput("year_ec", "Input a year", 2004, min = 2002, max = 2004),
                              selectInput("Semester",label = "Choose a Semester", choices = c(1,2)),
                              selectInput("Grd",label="Select a Grade", choices=c(9,10,11,12)),
                              selectizeInput("Subject",label = "Select an Exam", choices = c(gsub("\\..*","",colnames(grd9)[5:16]),gsub("\\..*","",colnames(grd12)[7:21])), selected = "Math")
                            ),                     
                            mainPanel(
                              fluidRow(
                                column(11,plotOutput("subject_plot")),
                                column(11,textOutput("pass_comment"))
                              )
                              
                            )
                   )
                 )
              ),
      # teachers
      tabPanel("Teacher",
               tabsetPanel(
                 tabPanel("Overview of Teachers",
                          h5("\n"),
                          fluidRow(
                            column(6,  plotlyOutput("plot_qual")
                            ),
                            column(6,  h5("\n"), plotOutput("plot_tch_to_stud")
                            ),
                            column(6, h5("\t\t\t1 = Below Grade 10,   2 = Grade 10,   4 = Grade 12,   5 = 1 Year higher education,")
                            ),
                            column(7),
                            column(6, h5("\t\t\t6 = 2 Year higher education, 7 = 3 Year higher education, 8=TTI, 10 =TTC, 14 = other")
                            ),
                            column(9, tableOutput("mean_state")
                            )
                          )
                          ),
                 tabPanel("School Conditions",
                          h5("\n"),
                          fluidRow(
                            column(12,  selectizeInput(
                              "School_tch",label="Select a School", choices=levels(staffed.expanded$SCHOOL), multiple = TRUE, selected = "Addibre")
                            ),
                            column(8, plotlyOutput("plot_qual2")),
                            column(4, h5("1 = Below Grade 10,   2 = Grade 10,   4 = Grade 12,\n"),
                                      h5("5 = 1 Year higher education, 6 = 2 Year higher education,\n"),
                                      h5("7 = 3 Year higher education, 8=TTI, "),
                                      h5("10 =TTC, 14 = other"),
                                      h1("\n"),
                                      verbatimTextOutput("tch_comment")
                            )
                          )  
                 )
               )
             )
    )
  )
)
