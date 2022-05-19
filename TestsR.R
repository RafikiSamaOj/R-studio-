#Load the shiny and dplyr packages
library(shiny)
library(dplyr)
library(ggplot2)
library(thematic)

#we get COVID data from our usual source and format it the way we did before
accident_dataset <- read.csv(file="data_2020-2015.csv", header=FALSE, stringsAsFactors=FALSE, fileEncoding="latin1",sep=";")
colnames(accident_dataset)<-c("ID","lettre_vehicule","Annee","Territoire","Type_accident","CNIT","cathegorie_du_vehicule","Age_du_vehicule")
data_for_selection <- accident_dataset %>% select(c(Age_du_vehicule,cathegorie_du_vehicule))
Annee_selection= c("2015","2016","2017","2018","2019","2020")


#create the user interface (ui), this time we specify more things:
ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  
  #we add a sidebar by calling sidebarLayout(), then we can specify what we want to put in the sidebar panel:
  sidebarLayout(
    sidebarPanel(width = 3,
                 varSelectInput(inputId="variable", label = "Select the variable", data_for_selection),
                 
                 selectInput("AnneeID", "Annee :", Annee_selection, selected = 2020, multiple = FALSE, selectize = TRUE, width = NULL, size = NULL)),
                 
                 
    mainPanel(width = 9,
              tabsetPanel(
                tabPanel("Notre projet",HTML("<H1> Notre application </H1>  <H3> Realisée par Raphael Colin, Maximilien Teil et Alexandre Florance </p>"),),
                #the user can pick a time range
                tabPanel("Nombre d'accidents mortels", plotOutput("plot"),),
                #we show the plot
                tabPanel("Repartition geographique", plotOutput("plot2"),)
              )
    )
  ))


#we store the server functions here - where we perform calculations based on the user's choices
server <- function(input, output) {
  
  #for the theme of the outputs to be consistent with the theme of the ui
  thematic_shiny()
  
  
  #check whether the user ticked the select box, and act accordingly
  #we define a reactive, something that reacts to the user's actions, 
  #that is we create a new dataframe called dataset based on the choices of the user
 
   #C'est a partir d'ici que ça peche // je n'arrive pas a creer les bons dataset avec les valeurs saisie (l'annee, le type de vehicule et age du vehicule)
  dataset <- reactive({
      accident_dataset  %>% filter(Annee==AnneeID())%>%
        select(c(!!input$variable,Type_accident))  %>%
        group_by(Type_accident) %>%
        summarize(sum_var=sum(!!input$variable))
  })
  
  #check whether the user ticked the select box, and act accordingly
  #we define a reactive, something that reacts to the user's actions, 
  #that is we create a new dataframe called dataset2 based on the choices of the user
  dataset2 <- reactive({
    accident_dataset  %>% filter(Annee==selectInput())%>%
      select(c(!!input$variable,Territoire))  %>%
      group_by(Territoire) %>%
      summarize(sum_var=sum(!!input$variable))
    
  })
  
  #plots based on user choices
  #gender plot
  output$plot <- renderPlot({
    ggplot(dataset(),aes(Type_accident,sum_var,fill=Type_accident)) +
      geom_col()+
      ggtitle(paste("Type d'accident par ",input$variable,"pour l'année ",input$date)) +
      labs(fill = "Type_accident",x="Type d'accident", y=input$variable) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  #time series plot
  output$plot2 <- renderPlot({
    ggplot(dataset2(),aes(Territoire,sum_var,fill=Territoire)) +
      geom_col()+
      ggtitle(paste("Accidents par Territoires ",input$variable,"pour l'année ",input$date)) +
      labs(fill = "Territoire",x="Territoire", y=input$variable) +
      theme(plot.title = element_text(hjust = 0.5))
  })
  
  output$summary <- renderPrint({
    summary(dataset_s())
  })
}
#we create the app
shinyApp(ui, server)



