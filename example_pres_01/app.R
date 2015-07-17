library(shiny)
library(shinyapps)
library(rmarkdown)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
source("doc/graphics.R")
source("doc/source_files_pres_01.R")

############################################################
## ui
############################################################

ui <- shinyUI(
        navbarPage(title = "", 
                   collapsible = TRUE, fluid = TRUE, 
                       tabPanel("Overview",
                             fluidRow(
                                  column(4,
                                    #selectizeInput('resource_type', 'Select type', choices = levels(total$Resource.Type), selected= "DEL", multiple = TRUE),
                                    selectizeInput('expense_type', 'Select type', 
                                                     choices = levels(total$Expense.Type), 
                                                     selected = "Departmental expenditure",
                                                     multiple = FALSE)
                                    ),
                                  column(8,
                                    #conditional based on the choice above
                                    uiOutput("activityControls")     
                                    )
                                  #column(4
                                    #radioButtons("format", "Document format", c("pdf", "HTML", "Word"), inline = TRUE),
                                    #downloadButton("downloadReport")
                                  #)
                            ),
                            fluidRow(
                                  column(6,
                                    plotOutput("chart1", height = "360px")
                                  ),
                                  column(6,
                                    plotOutput("chart2", height = "360px")
                                  )
                            )
                       ),
                       tabPanel("Summary",
                                #verbatimTextOutput("summary")
                                fluidRow(
                                  column(6,
                                         plotOutput("chart3", height= "220px")),
                                  column(6,
                                         plotOutput("chart4", height= "220px")
                                         )),
                                  fluidRow(
                                  column(6,
                                         plotOutput("chart6", height= "220px")),
                                  column(6,
                                         plotOutput("chart5", height= "220px")
                                         )) 
                       ),
                       tabPanel("Data", 
                                downloadButton("downloadData", "Download"),
                                HTML("<p> <br></p>"),
                                dataTableOutput("table")
                               )
                   )
  )
  
#################################################################
## server
#################################################################
server <- shinyServer(function(input, output){
  
  output$activityControls <- renderUI({
    exp <- input$expense_type
    selectizeInput('activity_type', 'Activity type', 
                   choices = levels(factor(total$Broad.Activity[total$Expense == exp])),
                   selected = "Arts, Heritage, Libraries", multiple = FALSE, width = "100%")
  })
  
  #overview section: bar plot
  output$chart1 <- renderPlot({
    
    exp <- input$expense_type #"Departmental expenditure" #
    act <- input$activity_type #"Arts, Heritage, Libraries" #
    total <- total[total$Expense.Type == exp & total$Broad.Activity == act, ]
    
    total <- aggregate(total$Spending.Value, by = list(total$Resource.Type, total$Expense.Type, total$Broad.Activity, total$Year), FUN = sum)
    colnames(total) <- c("Resource", "Expense", "Activity", "Year", "Value")
    tbl <- total[ order(-total[,5]), ]
    tbl$Value <- tbl$Value/1e6
    
    ggplot(data = tbl, aes(x = Year, y = Value)) + 
      geom_bar(stat = "identity", fill= "#BD0026") +
      fte_theme()  +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") +
      ggtitle("Spending For Chosen Expenses and Activities")
  })
  
  #overview section: boxplot
  output$chart2 <- renderPlot({
    
    exp <- input$expense_type #"Departmental expenditure" #
    act <- input$activity_type #"Arts, Heritage, Libraries" #
    total <- total[total$Expense.Type == exp & total$Broad.Activity == act, ]
    total$Spending.Value <- total$Spending.Value/1e6 
    
    ggplot(data = total, aes(x = Year, y = Spending.Value)) +
      geom_boxplot(outlier.colour = "grey60", colour= "grey60", fill= "#BD0026") + 
      fte_theme()  +
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") + 
      ggtitle("Spending For Chosen Expenses and Activities")
  })
  
  #summary section: bar plot
  output$chart3 <- renderPlot({
    
    spend_totals$SpendingTotals <- spend_totals$SpendingTotals/1e6
    
    ggplot(data = spend_totals, aes(x = Year, y = SpendingTotals)) + 
      geom_bar(stat = "identity", fill= "#BD0026") +
      fte_theme()  +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") +
      ggtitle("Spending Across All Activities")
  })
  
  #summary section: diff plot
  output$chart4 <- renderPlot({
    
    totals_diff <- data.frame(diff(as.matrix(spend_totals$SpendingTotals)))
    spend_totals_diff <- cbind(spend_totals[-1,], totals_diff)
    colnames(spend_totals_diff)[3] <- c("Diff")
    spend_totals_diff$Diff <- spend_totals_diff$Diff/1e6
    
    ggplot(data = spend_totals_diff, aes(x = Year, y = Diff)) + 
      geom_bar(stat = "identity",  fill= "#BD0026") +
      fte_theme()  +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") +
      ggtitle("Spending Differences in Consecutive Years")
    
  })
  
  #summary section: scaled boxplot
  output$chart5 <- renderPlot({
    total <- total[, 5:6]
    total$Spending.Value <- total$Spending.Value/1e6
    
    p0 <- ggplot(data = total, aes(x = Year, y = Spending.Value)) +
      geom_boxplot(outlier.colour = "grey60", colour= "grey70",  fill= "#BD0026") + 
      fte_theme()  +
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") +
      ggtitle("Scaled Limits of Spending Values\nZoomed Out 5% of Data Points (Outliers)")
    
    ylim1 <- boxplot.stats(total$Spending.Value)$stats[c(1, 5)]
    # scale y limits based on ylim1
    p0 + coord_cartesian(ylim = ylim1 + c(-0.05, 0.05) * diff(ylim1)/2)
    
  })
  
  #summary section: boxplot
  output$chart6 <- renderPlot({
    total <- total[, 5:6]
    total$Spending.Value <- total$Spending.Value/1e6
    
    ggplot(data = total, aes(x = Year, y = Spending.Value)) +
      geom_boxplot(outlier.colour = "grey60", colour= "grey70",  fill= "#BD0026") + 
      fte_theme()  +
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(labels = comma) + 
      ylab("Spending Values (£ Billion)") +
      ggtitle("Spending Across All Activities")
    
  })
  
  output$table <- renderDataTable({
    colnames(total)[6] <- "Spending.Value(£'000s)"
    total
  }, options = list(aLengthMenu = c(10, 25, 50, 100, 150), iDisplayLength = 2)
  )
  
  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('DCMS Spending Trends', sep = '.', 
            switch(input$format, Word = 'docx', HTML = 'html')
      )
    },
    content = function(file) {
      src <- normalizePath('doc/spending_report_pres_01.Rmd')
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      #owd <- setwd(tempdir())
      #on.exit(setwd(owd))
      file.copy(src, 'doc/spending_report_pres_01.Rmd')
      library(rmarkdown)
      out <- render('doc/spending_report_pres_01.Rmd', 
                    switch(input$format,  
                           HTML = html_document(), 
                           Word = word_document()))
      file.rename(out, file)
    }
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {"DCMS_spending_data.csv"},
    content = function(file) {write.csv(total, file)
    })
  
  
}) # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)