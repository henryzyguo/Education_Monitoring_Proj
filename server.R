library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)

enr_age <- readRDS("enr_age.Rdata")
staff_ed <- readRDS("staff_ed.Rdata")
ent_new <-readRDS("ent_new.RData")
grd9 <- readRDS("grd9.RData")
grd12 <- readRDS("grd12.RData")
staffed.expanded <- readRDS("staffed.expanded.RData")
acad_overview <- readRDS("aca_overview.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  observe({
    # validate(
    #   need(nrow(G1)>0, "Sorry, the data is not accessible")
    # )
    
    output$ageplot <- renderPlotly({
      enr_age<-filter(enr_age,SCHOOL==input$School) 
      G1<-select(enr_age,starts_with(paste("G",input$Grade, sep = "")),Acad_yr)
      #G1F<-select(enr_age,starts_with("G1F"),Acad_yr)
      G1<-arrange(G1,desc(Acad_yr))
      G1$Acad_yr <-NULL 
      rownames(G1)<-(2004:2010)
      G1<-as.table(t(G1))
      dat2 <- filter(melt(G1),value!=0)
      sex1<-substr(dat2$Var1,3,3)
      age1<-substr(dat2$Var1,4,5)
      dat2<-cbind(dat2,sex1,age1)
      dat2$age1 <- as.numeric(as.character(dat2$age1))
      dat2 <- setNames(dat2, c("group","Year","Number","sex", "age"))
      vis <- ggplot(dat2, aes(x=sex, y = Number, fill=as.factor(age))) + 
        geom_bar(stat="identity") +
        labs(x="year",y="person number",fill="age",title="Age component each year")+
        scale_fill_manual(values=c("indianred", "orange", "gold","skyblue4","light blue","blue","red","yellow","purple"))+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(.~Year)
      ggplotly(vis, tooltip = c("Year","Number"))
    }) 
    
    output$new_ageplot <- renderPlotly({
      ent_new <-filter(ent_new,SCHOOL==input$School)
      ent_new <-select(ent_new,4:12)
      rownames(ent_new)<-c(2006,2010)
      ent_new <-select(ent_new,-Acad_yr)
      #melt
      dat3 <- filter(melt(t(ent_new)),value!=0)
      vis2 <- ggplot(dat3, aes(x=Var2, y=value, fill=as.factor(Var1))) + 
                     geom_bar(stat="identity",width = 0.5) +
                     labs(x="year",y="person number",fill="age")+
                     scale_fill_manual(values=c("indianred", "orange", "gold","skyblue4","light blue","blue"))+
                     theme(plot.title = element_text(hjust = 0.5))
      ggplotly(vis2, tooltip = "value")
    })
    
    output$new_sexplot <- renderPlotly({
      ent_new <-filter(ent_new,SCHOOL==input$School)
      ent_new <-select(ent_new,4:12)
      rownames(ent_new)<-c(2006,2010)
      ent_new <-select(ent_new,-Acad_yr)
      #melt
      dat3 <- melt(t(ent_new))
      dat3 %>% separate(Var1,c("male","age"))
      sex<-substr(dat3$Var1,1,1)
      age<-substr(dat3$Var1,2,4)
      dat3<-cbind(dat3,sex,age)
      dat3$sex <- as.factor(dat3$sex)
      vis3 <- ggplot(dat3, aes(x=age, y=value,fill=sex)) + geom_bar(stat="identity") +
        labs(x="age",y="person number",title="New entrant each year",fill="sex")+
        scale_fill_manual(values=c("indianred", "orange", "gold","skyblue4","light blue","blue"))+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(.~Var2)
    })
    output$enr_comment <- renderText(
"This visualization shows the age distribution by age in each year from 2004-2010 (Ethiopian Calendar), in one specific school and grade. You can explore different schools and grades.\n
From this visualization, we are able to compare age and sex distribution among Academic Years. We can find that during 2004 and 2010, more girls are joining the school while the enrollment fluctuation remains high.\n
A reasonable deduction is that a number of students dropped out of school between 2004 and 2006 but they returned school in 2007 and 2008. This can explain the significant increase of elder-age-students in those years.")
    
    output$ent_comment1 <- renderText("This visualization shows the age distribution of new entrants in 2006&2010 (Ethiopian Calendar), in one specific school. There are choices of different school to select.")
    
    output$ent_comment2 <- renderText("We are able to note the sex ratio in different age and compare the age distribution in different year. (we only have data of new entrants in 2006&2010, it can show a clearer trend in the future if more data is added.)")
    
  })
  
  # for teachers
  observe({
    output$plot_qual <- renderPlotly({
      # staffed.expanded <- staffed.expanded[(staffed.expanded$SCHOOL==as.character(input$School_tch)),]
      staff_means <- ddply(staffed.expanded, "grade", summarise, mean_Q = mean(Qualification))
      violin <- ggplot(staffed.expanded, aes(x=grade, y=Qualification, fill=grade, color=grade)) + 
        geom_violin(show.legend = TRUE) +
        labs(title="Teachers' Qualification by Grades",x="Teaching Grades", y = "Years of Education after Grade 10") +
        facet_grid(~grade,scale ="free")
      violin + geom_hline(data=staff_means, aes(yintercept=mean_Q), color="orange", linetype="dashed",show.legend = TRUE)+
        scale_fill_manual(values=c("indianred", "gold"))+
        scale_color_manual(values=c("indianred", "gold"))
    })
    
    output$plot_tch_to_stud <- renderPlot({
      enr_age$Std_Boy <- rowSums(enr_age[grep("M",colnames(enr_age))])
      enr_age$Std_Girl <- rowSums(enr_age[grep("F",colnames(enr_age))])
      # add a total number of teachers
      staff_ed$total <- rowSums(staff_ed[,5:26])
      tch_std <- enr_age[,c(3,4,183,184,185)] %>% inner_join(staff_ed[,c(3,4,27)], by = c("SCHOOL","Acad_yr"))
      # tch_std <- melt(tch_std, id=c("SCHOOL","Acad_yr","Tch_Sum"))
      p<-ggplot(data=tch_std, aes(x=SCHOOL, y=Tab_16/total, fill=SCHOOL)) +
        geom_bar(stat="identity",width=0.2)+
        labs(title="Students/Teachers Ratio in 2006",x="School Name", y = "Average Student Number of A Teacher") 
      # Horizontal bar plot
      p + coord_flip()+
        geom_text(aes(label=round(Tab_16/total,2),nudge_y = 1))
    })
  
    output$plot_qual2 <- renderPlotly({
      staffed.expanded2 <- staffed.expanded[(staffed.expanded$SCHOOL==input$School_tch),]
      staff_means <- mean(staffed.expanded2$Qualification, na.rm = TRUE)
      staffed.expanded2$Qualification <- as.factor(as.character(staffed.expanded2$Qualification))
      ggplot(data = staffed.expanded2, aes(Qualification, fill=grade, color=grade)) + 
        geom_histogram(stat = 'count', show.legend = TRUE, width = 0.25) +
        labs(title="Teachers' Qualification by Grades",x="Teaching Grades", y = "Years of Education after Grade 10") +
        scale_fill_manual(values=c("indianred", "gold"))+
        scale_color_manual(values=c("indianred", "gold"))+
        coord_cartesian(xlim = c(1, 14))
    })
    
    
    output$tch_comment <- renderText({
"This visualization shows the distribution of teachers’ certification starting in 2006, in one specific school.  
Most teachers of lower education levels are teaching grade 1 to 4 students and the great proportion of teachers have not received higher education"
    })
    
    output$mean_state <- renderTable({
      staff_means <- ddply(staffed.expanded, "grade", summarise, mean_Q = mean(Qualification))
      colnames(staff_means)[2]<-"Mean of Teachers' Qualification Level"
      staff_means
    })
    
    
  })
  
  
# for academics
  observe({
    # academic overview
    output$acadplot <- renderPlotly({
      # acad_overview <- filter(acad_overview,Semester=='1')
      acad_overview <- select(acad_overview,-Semester)
      acad_overview <- melt(acad_overview)
      vis4 <- ggplot(acad_overview, aes(x=variable,y=value,fill=variable)) + geom_bar(stat="identity")+
        scale_fill_manual(values=c("indianred", "orange", "gold","skyblue4","light blue","blue"))+
        labs(x="grade",y="score",title="Academic overview each year",fill="subject")+
        theme(plot.title = element_text(hjust = 0.5))+
        facet_grid(.~reorder(grade,value))
      ggplotly(vis4, tooltip = c("value"))
    })
    
    output$acad_overview <- renderText(
"This graph shows the average academic scores of five main subjects from 2002-2004 in School Masho. 
If we compare the academics from grade 9-12, we can find an significant rise in P.E. grades but other subjects remains stable.")
    
    output$pass_comment <- renderText(
"You can filter the time, grade and subject to view the distribution of students' grades by sex. The vertical red broken line is the passing score of 50.
An concerning discovery is that in Grade 9 and 10, the majority of top marks are from male students and male students' overall academic performance is better than women."
    )
    

    # grade distribution by subject 
    output$subject_plot <- renderPlot({
      # select table and Grades
      if((input$Grd==12)|(input$Grd==11)){
        selected <- grd12[grepl(input$Grd,colnames(grd12))]
      }
      else selected <- grd9[(grepl(input$Grd,colnames(grd9))|colnames(grd9)=="Sex")]
      # rename the Academic Year
      colnames(selected)[1]<- 'Acad_yr'
      selected <- selected[selected$Acad_yr==input$year_ec, # select Acad year
               grepl(paste('Sem',input$Semester,sep = ""),colnames(selected))& # select a semester
                                                  grepl(input$Subject,colnames(selected))|
                                                  colnames(selected)=="Acad_yr"|
                                                  colnames(selected)=="Sex"]
      colnames(selected)[2] <- "score"
      selected <- selected %>%  subset(score !='NA')
      # create a table to save pass rate
      pass <- length(selected$score[selected$score >= 50])/length(selected$score)
      # print(cat('passing rate is',pass[1,2]))
      if(input$Grd==9|input$Grd==10){
        p  <- ggplot(selected, aes(score, fill=Sex, color=Sex))+
          scale_color_manual(values=c("indianred", "gold"))
        p + geom_density(alpha=0.5)+
          scale_fill_manual(values=c("indianred", "gold"))+
          geom_vline(data=selected, aes(xintercept=50), color="red", linetype="dashed",show.legend = TRUE)+
          geom_text(aes(55, 0.001, label=paste("Passing Rate: ", round(pass,4))))+
          labs(title = "Students Marks Distribution", x="Marks", y = "Student Percentage Density") #+
          # coord_cartesian(xlim = c(0, 100))
      }
      else {
        p  <- ggplot(selected, aes(score,fill="gold", color="gold"),show.legend = FALSE)
        p + geom_density(alpha=0.5,show.legend = FALSE)+
          scale_fill_manual(values="gold")+
          scale_color_manual(values="gold")+
          geom_vline(data=selected, aes(xintercept=50), color="red", linetype="dashed",show.legend = FALSE)+
          geom_text(aes(55, 0.001, label=paste("Passing Rate: ", round(pass,4))),color = "red")+
          labs(title = "Students Marks Distribution",x="Marks", y = "Student Percentage Density") #+
          # coord_cartesian(xlim = c(0, 100))
      }

    })
    
    
    
  })
  
  
})
