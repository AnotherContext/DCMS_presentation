library(shiny)
library(shinyapps)
library(rmarkdown)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
source("doc/graphics.R")
source("doc/source_files_pres_02.R")
source("doc/source_files_pres_03.R")

############################################################
## ui
############################################################

ui <- shinyUI(
  navbarPage(title = "", 
             collapsible = TRUE, fluid = TRUE, 
             tabPanel("Sizeband",  
                      fluidRow(
                        column(12,
                              selectizeInput("desc", "Select", 
                                             choices = levels(factor(emp$Desc)),
                                             selected = "Arts, entertainment and recreation ",
                                             multiple = FALSE))
                      ),
                      fluidRow(
                          column(12,
                                plotOutput("chart1", height = "350px"))
             )),
             tabPanel("Costs",
                      fluidRow(
                          column(12,
                                 plotOutput("chart2", height = "420px"))
             ))
  )
)

#################################################################
## server
#################################################################
server <- shinyServer(function(input, output){
  
  #overview section: abs bar plot
  output$chart1 <- renderPlot({
    emp <- emp[ emp$variable != "No of Enterprises", ]
    emp$value <- emp$value/1e3
    desc <- input$desc #"Arts, entertainment and recreation " 
    emp <- emp[ emp$Desc == desc,]
    
    ggplot(data = emp, aes(x = Year, y = value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Sizeband")) +
      fte_theme()  +
      facet_grid( variable  ~ Sizeband) +
      scale_fill_brewer(palette = "Set2", breaks = c("1 to 9", "10 to 49", "50 to 249", "250 and over")) +
      scale_y_continuous(labels = comma) +
      ylab("Value (£ Billion)") +
      ggtitle("The Annual Business Survey for Arts, Entertainment and Recreation, Information and Communication\ncovering the Non-Financial Business Economy which accounts for approx. 75% of the UK economy in terms of GVA*") +
      theme(legend.position="top") +
      guides(fill=guide_legend(title= c("No. of Employess"))) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
  })
  
  #overview section: no of ent bar plot
  output$chart2 <- renderPlot({
    emp <- emp[ emp$variable == "No of Enterprises", ]
    emp$value <- emp$value/1e3
    
    #desc <- input$desc #"Arts, entertainment and recreation " 
    #emp <- emp[ emp$Desc == desc,]
    
    ggplot(data = emp, aes(x = Year, y = value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Sizeband")) +
      fte_theme()  +
      facet_grid( Sizeband  ~ Desc) +
      scale_fill_brewer(palette = "Set2", breaks = c("1 to 9", "10 to 49", "50 to 249", "250 and over")) +
      #scale_y_discrete(labels = comma)+
      ylab("No. of Enterprises (Thousand)") +
      ggtitle("The Annual Business Survey for Arts, Entertainment and Recreation, Information and Communication\ncovering the Non-Financial Business Economy which accounts for approx. 75% of the UK economy in terms of GVA*") +
      theme(legend.position="top") +
      guides(fill=guide_legend(title= c("No. of Employees"))) + 
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  
}) # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)