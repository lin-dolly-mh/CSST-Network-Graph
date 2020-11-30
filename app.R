#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(readxl)
library(janitor)
library(lubridate)
library(ggpattern)
#remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)


# Define UI for plots from uploaded excel
ui <- fluidPage(

    # Application title
    titlePanel("Outbreak Visualizations"),

    # Sidebar with inputs (file, school, case selection) 
    sidebarLayout(
        sidebarPanel(
            fileInput('file1', 'Choose Excel file to load)',
                      accept = c(".xls", ".xlsx", ".xlsm"))
        ),
        
        # Show a plot
        mainPanel(
            tabsetPanel(type="tabs",
                        tabPanel("EpiCurve", 
                                 fluidRow(plotOutput("EpiPlot")), 
                                 fluidRow(tableOutput("EpiTable"))),
                        tabPanel("Network", 
                                 fluidRow(plotOutput("NetworkPlot")),
                                 fluidRow(tableOutput("NetworkTable"))),
                        tabPanel("Gantt", 
                                 fluidRow(plotOutput("GanttPlot")),
                                 fluidRow(tableOutput("GanttTable")))                        
            )
        )
    )
)

    
# Define server logic required to draw plots
server <- function(input, output, session) {
    
    output$EpiPlot <- renderPlot({
        req(input$file1)

        data <- read_excel(input$file1$datapath, sheet = "Cases")
        controls <- read_excel(input$file1$datapath, sheet = "Controls")
        data <- data %>% clean_names()
        controls <- controls %>% clean_names()
        
        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        Plot_all<- sample %>%
            ggplot() +
            geom_bar_pattern(aes(x=episode,
                                 pattern=HRC,
                                 fill=client_or_staff_select),
                             pattern_fill = "black",
                             pattern_density = 0.1,
                             pattern_spacing = 0.02,
                             width = 86400) +
            scale_pattern_manual(values = c("crosshatch", "circle", "stripe", NA)) +
            theme_bw()+
            scale_x_datetime(date_breaks = "day") +
            labs(title ="Epi Curve of All Cases", x = "Episode Date", y = "Count",
                 fill = "Staff or Client", pattern="case is a HRC of COVID at") +
            theme(axis.text.x.bottom = element_text(angle = 90, hjust = 0.5, vjust = 0.5))+
            geom_vline(data = controls, mapping = aes(xintercept = control_start_date_yyyy_mm_dd)) +
            geom_text(data = controls, mapping = aes(x = (control_start_date_yyyy_mm_dd), label = controls), y = 2.5, angle=90)
        Plot_all
    })
    
    output$EpiTable <- renderTable({
        req(input$file1)
        data <- read_excel(input$file1$datapath, sheet = "Cases")
        data <- data %>% clean_names()
        
        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        sample%>%
            select(cod_id,
                   episode,sx_yes_no,
                   client_or_staff_select, client_or_staff_cohort)%>%
            arrange(episode)%>%
            mutate(episode = format.Date(episode, format = "%Y-%m-%d"))
    })
    
    output$NetworkPlot <- renderPlot({
        req(input$file1)
        
        data <- read_excel(input$file1$datapath, sheet = "Cases")
        data <- data %>% clean_names()

        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        nodes <- sample%>%select(cod_id,client_or_staff_select,client_or_staff_cohort,HRC)
        colnames(nodes) <-c("id", "type","cohort","HRC")
        nodes2 <- data%>%select(other_hr_contact_cod_number,other_hr_contact_relationship,off_site_location)
        colnames(nodes2) <-c("id", "type","cohort")
        nodes2$HRC<-"home"
        nodes3 <- nodes%>%rbind(nodes,nodes2)
        nodes3$id<-as.character(nodes3$id)
        colnames(nodes3) <- c("id", "type","cohort","HRC")
        #cleaning multiple cod ids together
        nodes3<- nodes3%>%separate_rows(id)
        nodes3<- distinct(nodes3)
        #may need to remove empty rows, combine labels for the same id eg student,sibling
        nodes4<- nodes3 %>%
            filter(!is.na(id)) %>%
            group_by(id)%>%
            summarise(label=paste(id,"\n",type,cohort,collapse=", "))
        #nodes5<- left_join(nodes4,nodes3,by='id')
        
        # NODE BY ID BY HRC TYPE
        #if case has HRC at home should be work if more likely acquired at work, home if more likely acquired at home
        #new node who are home contacts should be home
        
        node_colors<-nodes3%>%
            dplyr::select(id,HRC) %>%
            mutate(HRC_colour = case_when(
                HRC=="home"~"light blue",
                HRC=="work"~"orange",
                HRC=="both"~"pink",
                HRC=="NA"~"gray")) 
        node_colors_all<-node_colors%>%filter(id%in%nodes4$id)
        
        nodes5<-nodes4%>%
            left_join(node_colors, nodes5, by="id") %>%
            select(id, label, HRC, HRC_colour)
        
        #Edges
        # EDGE
        ## select by ID, HR contacts #directionality to be visited later
        ## if COD# in HR contact at school, other HR contact, in school or non school
        edges <- data%>%select(cod_id,case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)
        colnames(edges) <- c("case", "contact")
        edges2 <- data%>%select(cod_id,other_hr_contact_cod_number)
        colnames(edges2) <- c("case", "contact")
        edges3 <- rbind(edges,edges2)
        edges3<- edges3%>%separate_rows(contact)
        #may need to remove those without both case and contact
        edges3<-edges3%>%filter(!is.na(contact))
        
        #Create graph
        library(igraph)
        net = graph_from_data_frame(edges3, vertices = nodes5, directed = F)
        plot(net, vertex.color=nodes5$HRC_colour)
        
        legend('right',
               legend = unique(nodes5$HRC),
               fill = unique(nodes5$HRC_colour),
               cex=0.50)
        
    })
    
    output$NetworkTable <- renderTable({
        req(input$file1)
        data <- read_excel(input$file1$datapath, sheet = "Cases")
        data <- data %>% clean_names()
        
        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        nodes <- sample%>%select(cod_id,client_or_staff_select,client_or_staff_cohort,HRC)
        colnames(nodes) <-c("id", "type","cohort","HRC")
        nodes2 <- data%>%select(other_hr_contact_cod_number,other_hr_contact_relationship,off_site_location)
        colnames(nodes2) <-c("id", "type","cohort")
        nodes2$HRC<-"home"
        nodes3 <- nodes%>%rbind(nodes,nodes2)
        nodes3$id<-as.character(nodes3$id)
        colnames(nodes3) <- c("id", "type","cohort","HRC")
        #cleaning multiple cod ids together
        nodes3<- nodes3%>%separate_rows(id)
        nodes3<- distinct(nodes3)
        #may need to remove empty rows, combine labels for the same id eg student,sibling
        nodes4<- nodes3 %>%
            filter(!is.na(id)) %>%
            group_by(id)%>%
            summarise(label=paste(id,"\n",type,cohort,collapse=", "))
        #nodes5<- left_join(nodes4,nodes3,by='id')
        
        node_colors<-nodes3%>%
            dplyr::select(id,HRC) %>%
            mutate(HRC_colour = case_when(
                HRC=="home"~"blue",
                HRC=="work"~"orange",
                HRC=="both"~"pink",
                HRC=="NA"~"gray")) 
        node_colors_all<-node_colors%>%filter(id%in%nodes4$id)
        
        nodes5<-nodes4%>%
            left_join(node_colors, nodes5, by="id") %>%
            select(id, label, HRC, HRC_colour)
        
        nodes5%>%select(label, HRC)
    })
    
    output$GanttPlot <- renderPlot({
        req(input$file1)
        
        data <- read_excel(input$file1$datapath, sheet = "Cases")
        controls <- read_excel(input$file1$datapath, sheet = "Controls")
        data <- data %>% clean_names()
        controls <- controls %>% clean_names()
        
        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        gantt<-sample%>%select(
            cod_id,
            client_or_staff_select,
            client_or_staff_cohort,
            sx_yes_no,
            symptom_onset_yyyy_mm_dd,
            test_date_yyyy_mm_dd,
            last_day_of_attendance_yyyy_mm_dd)
        
        gantt <- gantt %>% mutate(inf_start = case_when(
            sx_yes_no=="yes"~((symptom_onset_yyyy_mm_dd)-days(2)),
            sx_yes_no=="no"~((test_date_yyyy_mm_dd) - days(10))),
            inf_end = case_when(
                sx_yes_no=="yes"~((symptom_onset_yyyy_mm_dd) + days(10)),
                sx_yes_no=="no"~((test_date_yyyy_mm_dd) + days(10))),
            label = paste(cod_id, client_or_staff_select, client_or_staff_cohort, sep =" "))
        
        ggplot(gantt)+
            geom_segment(aes(y=fct_reorder(label, test_date_yyyy_mm_dd, .desc=TRUE), 
                             yend = fct_reorder(label, test_date_yyyy_mm_dd, .desc=TRUE), 
                             x = inf_start, xend = inf_end, colour=sx_yes_no),size = 5,alpha=0.6) +
            geom_point(aes(y=label, x=symptom_onset_yyyy_mm_dd), shape='S')+
            geom_point(aes(y=label, x=test_date_yyyy_mm_dd), shape='T')+
            geom_point(aes(y=label, x=last_day_of_attendance_yyyy_mm_dd), shape='L')+
            #geom_label(data = df_for_labels, aes(label=class, y = person, x=date), size=2) +
            geom_vline(data = controls, mapping = aes(xintercept = control_start_date_yyyy_mm_dd)) +
            geom_text(data = controls, mapping = aes((x = control_start_date_yyyy_mm_dd), 
                                                     label = controls), y = 2.5, angle=90,  size = 3)+
            xlab("Date") +
            ylab("Case") +
            ggtitle("Gantt Graph")+
            scale_x_datetime(date_breaks = "day", date_labels = "%D") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5)) +
            #facet_grid(rows = vars(class), scales = "free") +
            geom_blank()
        
    })
    
    output$GanttTable <- renderTable({
        req(input$file1)
        
        data <- read_excel(input$file1$datapath, sheet = "Cases")
        controls <- read_excel(input$file1$datapath, sheet = "Controls")
        data <- data %>% clean_names()
        controls <- controls %>% clean_names()
        
        sample<-data%>%select(cod_id, 
                              client_or_staff_select, 
                              client_or_staff_cohort,
                              sx_yes_no, 
                              symptom_onset_yyyy_mm_dd,
                              test_date_yyyy_mm_dd,
                              last_day_of_attendance_yyyy_mm_dd,
                              case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number,
                              other_hr_contact_cod_number, 
                              other_hr_contact_relationship,
                              off_site_location
        ) %>%
            mutate(cod_id = as.factor(cod_id),
                   sx_yes_no = as.factor(sx_yes_no))
        
        sample<-sample%>%
            mutate(episode=case_when(
                !(is.na(symptom_onset_yyyy_mm_dd))~symptom_onset_yyyy_mm_dd,
                is.na(symptom_onset_yyyy_mm_dd)~test_date_yyyy_mm_dd),
                HRC=
                    case_when(
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number))) 
                        & !(is.na(as.factor(other_hr_contact_cod_number)))~"both",
                        !(is.na(as.factor(case_of_whom_they_are_a_hr_contact_at_outbreak_cod_number)))~"work",
                        !(is.na(as.factor(other_hr_contact_cod_number)))~"home"))
        
        gantt<-sample%>%select(
            cod_id,
            client_or_staff_select,
            client_or_staff_cohort,
            sx_yes_no,
            symptom_onset_yyyy_mm_dd,
            test_date_yyyy_mm_dd,
            last_day_of_attendance_yyyy_mm_dd)
        
        gantt <- gantt %>% mutate(inf_start = case_when(
            sx_yes_no=="yes"~((symptom_onset_yyyy_mm_dd)-days(2)),
            sx_yes_no=="no"~((test_date_yyyy_mm_dd) - days(10))),
            inf_end = case_when(
                sx_yes_no=="yes"~((symptom_onset_yyyy_mm_dd) + days(10)),
                sx_yes_no=="no"~((test_date_yyyy_mm_dd) + days(10))),
            label = paste(cod_id, client_or_staff_select, client_or_staff_cohort, sep =" "))
        
        gantt%>%select(
            cod_id,
            client_or_staff_select,
            client_or_staff_cohort,
            sx_yes_no,
            symptom_onset_yyyy_mm_dd,
            test_date_yyyy_mm_dd,
            last_day_of_attendance_yyyy_mm_dd)%>%
            mutate(symptom_onset_yyyy_mm_dd = format.Date(symptom_onset_yyyy_mm_dd, format = "%Y-%m-%d"),
               test_date_yyyy_mm_dd = format(test_date_yyyy_mm_dd, format = "%Y-%m-%d"),
               last_day_of_attendance_yyyy_mm_dd = format(last_day_of_attendance_yyyy_mm_dd, format = "%Y-%m-%d"))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
