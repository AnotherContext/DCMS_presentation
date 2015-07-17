library(shiny)
library(shinyapps)
library(rmarkdown)
library(ggplot2)
library(reshape2)
library(scales)
library(grid)
source("doc/graphics.R")
source("doc/source_files_pres_02.R")
 
############################################################
## ui
############################################################

ui <- shinyUI(
        navbarPage(title = "", 
                   collapsible = TRUE, fluid = TRUE, 
                       tabPanel("Overview",  
                                fluidRow(
                                  column(6,
                                         selectizeInput("user_type", "Select type", 
                                                            choices = c("Arts", "Leisure","Media", "Internet & Telecoms", "Gambling"), 
                                                            selected = "Arts",
                                                            multiple = FALSE))
                                  ),
                                
                              
                                 conditionalPanel(
                                    condition = "input.user_type == 'Arts'",
                                        fluidRow(
                                            column(6,
                                                   plotOutput("chart1", height= "200px")),
                                            column(6,
                                                   plotOutput("chart4", height= "200px"))
                                                   #plotOutput("chart5", height= "100px")
                                            
                                        ),
                                        fluidRow(
                                            column(6,
                                                   plotOutput("chart2", height= "200px")),
                                            column(6,
                                                   plotOutput("chart3", height= "200px"))
                                        )
                                 ),
                             
                                 conditionalPanel(
                                    condition = "input.user_type == 'Leisure'",
                                    
                                        fluidRow(
                                          column(6,
                                                 plotOutput("chart6", height= "200px")),
                                          column(6
                                                # plotOutput("chart7", height= "200px")
                                          )),
                                        fluidRow(
                                          column(6,
                                                 plotOutput("chart8", height= "200px")),
                                          column(6,
                                                 plotOutput("chart9", height= "200px")
                                          ))
                                ),
                                
                                conditionalPanel(
                                  condition = "input.user_type == 'Media'",
                                  
                                      fluidRow(
                                          column(6,
                                                 plotOutput("chart10", height= "200px")),
                                          column(6,
                                                 plotOutput("chart11", height= "200px"))
                                      ),
                                      fluidRow(
                                          column(6,
                                                 plotOutput("chart13", height= "200px")),
                                          column(6
                                      ))
                                ),
                                
                                conditionalPanel(
                                  condition = "input.user_type == 'Internet & Telecoms'",
                                  
                                      fluidRow(
                                          column(6,
                                                 plotOutput("chart16", height= "200px")),
                                          column(6,
                                                 plotOutput("chart17", height= "200px"))
                                      ),
                                      fluidRow(
                                          column(6
                                                 ),
                                          column(6
                                                 )
                                      )
                               ),
                               
                               conditionalPanel(
                                 condition = "input.user_type == 'Gambling'",
                                 
                                     fluidRow(
                                      column(6,
                                             plotOutput("chart14", height= "200px")),
                                      column(6,
                                             plotOutput("chart15", height= "200px")
                                      )),
                                    fluidRow(
                                      column(6,
                                             plotOutput("chart12", height= "200px")),
                                      column(6
                                      ))
                               )
                        ),
                       tabPanel("Annual Business Survey",
                               plotOutput("chart18", height = "420px")
                       )
                   )
  )
  
#################################################################
## server
#################################################################
server <- shinyServer(function(input, output){
  
  #art section: inc bar plot
  output$chart1 <- renderPlot({
    
    inc <- inc[,3:4]
    inc <- aggregate(inc$Value, by = list(inc$Year), FUN = sum)
    colnames(inc) <- c("Year", "Value")
    inc$Value <- inc$Value/1e6
    
    ggplot(data = inc, aes(x = Year, y = Value)) + 
      geom_bar(stat = "identity", fill = brewer.pal("Greens", n=9)[7]) + # aes_string(fill= "Year")) +
      fte_theme()  +
      #scale_fill_brewer(palette="Greens") + 
      scale_y_continuous(labels = comma) + 
      ylab("Income (£ Million)") +
      ggtitle("Museums & Galleries Income")
  })
  
  #art section: inc boxplot
  output$chart2 <- renderPlot({
    
    inc <- inc[,c(1,3:4)]
    #inc <- aggregate(inc$Value, by = list(inc$Income.Type, inc$Year), FUN = sum)
    colnames(inc) <- c("Income.Type", "Year", "Value")
    inc$Value <- inc$Value/1e6
    
    ggplot(data = inc, aes(x = Year, y = Value) )+
      geom_boxplot(outlier.colour = "grey60", colour= "grey60", fill = brewer.pal("Greens", n=9)[5]) + 
      fte_theme()  +
      facet_grid(Income.Type ~ .) +
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(breaks=c(0,50,90), labels = comma) + 
      ylab("Income (£ Million)") +
      ggtitle("Museums & Galleries Income by Type") 
    
  })
  
  #art section: scaled inc boxplot
  output$chart3 <- renderPlot({
    
    inc <- inc[,c(1,3:4)]
    colnames(inc) <- c("Income.Type", "Year", "Value")
    inc$Value <- inc$Value/1e6
    
    p0 <- ggplot(data = inc, aes(x = Year, y = Value)) +
      geom_boxplot(outlier.colour = "grey60", colour= "grey60", fill = brewer.pal("Greens", n=9)[5]) + 
      fte_theme()  +
      facet_grid(Income.Type ~ .) +
      scale_fill_brewer(palette="Greens") +   
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(breaks=c(0,5,9), labels = comma) + 
      ylab("Income (£ Million)") +
      ggtitle("Museums & Galleries Income by Type\nwith 5% of Data Points Scaled Out (Outliers)") 
    
    ylim1 <- boxplot.stats(inc$Value)$stats[c(1, 5)]
    # scale y limits based on ylim1
    p0 + coord_cartesian(ylim = ylim1 + c(-0.05, 0.05) * diff(ylim1)/2)
  })
  
  #art section: visits boxplot
  output$chart4 <- renderPlot({
    
    visit <- visit[, 2:3]
    visit <- aggregate(visit$Value, by = list(visit$Year), FUN = sum)
    colnames(visit) <- c("Year", "Value")
    visit$Value <- visit$Value/1e6
    
    ggplot(data = visit, aes(x = Year, y = Value)) + 
      geom_bar(stat = "identity",  fill= brewer.pal("Greens", n=9)[6]) + 
      fte_theme()  +
      #scale_fill_brewer(palette="Greens") + 
      scale_y_continuous(labels = comma) + 
      ylab("No. of Visits (Million)") +
      ggtitle("Museums & Galleries Visits")
    
  })
  
  #art section: visits diff plot
  output$chart5 <- renderPlot({
    
    visit <- visit[, 2:3]
    visit <- aggregate(visit$Value, by = list(visit$Year), FUN = sum)
    diff_v <- diff(visit$x)
    visit <- cbind(visit[-1,], diff_v)
    colnames(visit) <- c("Year", "Value", "Diff")
    visit$Value <- visit$Value/1e6
    
    ggplot(data = visit, aes(x = Year, y = Diff)) + 
      geom_bar(stat = "identity",  fill= brewer.pal("Greens", n=9)[6]) +
      fte_theme()  +
      #scale_fill_brewer(palette="Greens") + 
      scale_y_continuous(labels = comma) + 
      ylab("No. of Visits") +
      ggtitle("Museums & Galleries Visits Differences in Consecutive Years")
    
  })
  
  #leisure section: tourism bar plot
  output$chart6 <- renderPlot({
    
    tourism <- aggregate(tourism$Visits, by = list(tourism$Year), FUN = sum)
    colnames(tourism) <- c("Year", "Value")
    tourism$Value <- tourism$Value/1e6
    
    ggplot(data = tourism, aes(x = Year, y = Value)) + 
      geom_bar(stat = "identity", fill= "#fa9fb5") +
      fte_theme()  +
      #scale_fill_brewer(palette="YlOrRd") + 
      scale_y_continuous(labels = comma) + 
      ylab("No. Of Visits (Million)") +
      ggtitle("Overseas Residents' Visits  to the UK")
    
  })
  
  #leisure section: tourism economic value
  output$chart7 <- renderPlot({
    
  })
  
  #leisure section: sport wider impact
  output$chart8 <- renderPlot({
    #ordering factors decreasingly
    sport1$Wider.impacts <- factor(sport1$Wider.impacts, 
                                   levels = sport1$Wider.impacts[order(-sport1$Wider.value)])
    sport1$Wider.value <- sport1$Wider.value/1e9
    
    ggplot(data = sport1, aes(x = Wider.impacts, y = Wider.value)) + 
      geom_bar(stat = "identity",  fill= "#980043") +
      fte_theme()  +
      #scale_fill_brewer(palette="YlOrRd") + 
      scale_y_continuous(labels = comma) + 
      ylab("Gross Value Added (£ Billion)") +
      ggtitle("Estimated Economic Impact of Sport in 2012")
    
  })
  
  #leisure section: sport wider impact
  output$chart9 <- renderPlot({
    
    sport2$Sport.Activity <- factor(sport2$Sport.Activity, 
                                    levels = sport2$Sport.Activity[order(-sport2$Gross.Value.Added)])
    sport2$Gross.Value.Added <- sport2$Gross.Value.Added/1e9  
    
    ggplot(data = sport2, aes(x = Sport.Activity, y = Gross.Value.Added)) + 
      geom_bar(stat = "identity", fill= "#980043") +
      fte_theme()  +
      scale_fill_brewer(palette="YlOrRd") + 
      scale_y_continuous(labels = comma) + 
      ylab("Gross Value Added (£ Billion)") +
      ggtitle("Est. Economic Impact of Sport by Activity in 2012")
    
  })
  
  #media section: box office bar plot
  output$chart10 <- renderPlot({
    
    movie <- movie[9:14,]
    movie$Year <- as.factor(movie$Year)
    
    ggplot(data = movie, aes(x = Year, y = BoxOffice)) + 
      geom_bar(stat = "identity",  fill= "#e34a33") +
      fte_theme()  +
      scale_colour_brewer(palette="Blues") +
      ylab("Box Office Gross (£ Million)") +
      ggtitle("UK Box Office Trends")  
  })
  
  #media section: market share line plot
  output$chart11 <- renderPlot({
    
    movie <- movie[9:14,]
    
    ggplot(data = movie, aes(x = Year, y = Studio)) + 
      geom_line(aes(color = "UK studio-backed films")) + geom_line(data = movie, aes(x = Year, y = Indep, color = "UK Independent films")) +
      fte_theme()  +
      ylab("Percentage (%)") +
      ggtitle("UK films' share of the UK theatrical market") +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size=7,color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  #media section: media in general?? economic share of the market
  output$chart12 <- renderPlot({
    
  })
  
  #telecom section: rev bar plot 
  output$chart13 <- renderPlot({
    
    com <- com[, c(1,8:12)] 
    colnames(com) <- c("Industry", "2009", "2010", "2011", "2012", "2013")
    com <- melt(com, id= c("Industry"), variable.name = "Year", value.name = "Value")
    
    ggplot(data = com, aes(x = Year, y = Value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Industry")) +
      fte_theme()  +
      scale_fill_brewer(palette = "RdGy") +
      ylab("Revenues (£ Billion)") +
      ggtitle("Telecoms Indutries' Revenues") +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  #telecom section: gambling bar plot 
  output$chart14 <- renderPlot({
    
    colnames(gambling) <- c("Industry", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015")
    gambling <- melt(gambling, id= c("Industry"), variable.name = "Year", value.name = "Value")
    gambling <- aggregate(gambling$Value, by = list(gambling$Year), FUN = sum)
    colnames(gambling) <- c("Year", "Rev")
    gambling$Rev <- gambling$Rev/1e3
    
    ggplot(data = gambling, aes(x = Year, y = Rev)) + 
      geom_bar(stat = "identity",  fill= "#4d004b") +
      fte_theme()  +
      scale_fill_brewer(palette="Purples") + 
      ylab("Gross Gambling Yield (£ Billion)") +
      ggtitle("Gambling Indutries' Revenues")  
    
  })
  
  #telecom section: rev boxplot across industries 
  output$chart15 <- renderPlot({
    
    colnames(gambling) <- c("Industry", "2010-2011", "2011-2012", "2012-2013", "2013-2014", "2014-2015")
    gambling <- melt(gambling, id= c("Industry"),  variable.name = "Year", value.name = "Value")
    colnames(gambling) <- c("Industry", "Year", "Rev")
    gambling$Rev <- gambling$Rev/1e3
    
    
    ggplot(data = gambling, aes(x = Year, y = Rev)) + 
      geom_boxplot(outlier.colour = "grey60", colour= "grey70", fill= "#4d004b") + 
      fte_theme()  +
      #scale_fill_brewer(palette="Purples") +   
      stat_summary(fun.y=mean, geom="point", colour = "white") +
      scale_y_continuous(labels = comma) + 
      ylab("Gross Gambling Yield (£ Billion)") +
      ggtitle("Gambling in Revenues")  
    
  })
  
  #telecom section: internet access
  output$chart16 <- renderPlot({
    
    net1 <- net1[, c(1,5:10)]
    colnames(net1) <- c("Year", "2009", "2010", "2011", "2012", "2013", "2014")
    net1 <- melt(net1)
    colnames(net1) <- c("Usage", "Year", "Perc")
    
    ggplot(data = net1, aes(x = Year, y = Perc)) + 
      geom_bar(stat = "identity", aes_string(fill= "Usage")) +
      fte_theme()  +
      scale_fill_brewer(palette = "Paired", breaks=c("Daily", "At least weekly", "Less than weekly", "Did not use in the last three months")) +
      ylab("Percentage (%)") +
      ggtitle("Internet Connection Types") +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  #telecom section: internet access
  output$chart17 <- renderPlot({
    net2 <- net2[, 1:4]
    colnames(net2) <- c("Year", "Type", "Age: 16-64", "Age: 65+")
    net2 <- melt(net2, id= c("Year", "Type"))
    colnames(net2) <- c("Year", "Access", "Group", "Perc")
    
    ggplot(data = net2, aes(x = Year, y = Perc)) + 
      geom_bar(stat = "identity", aes_string(fill= "Access")) +
      fte_theme()  +
      facet_grid( Group  ~ .) +
      scale_fill_brewer(palette = "Set2") +
      #scale_y_discrete(breaks = c(0,50,100)) +
      ylab("Percentage (%)") +
      ggtitle("Internet Access") +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
  #abs section: abs bar plot
  output$chart18 <- renderPlot({
    abs <- abs[, c(1:2,4:5,10)]
    colnames(abs) <- c("Desc", "Year", "Turnover", "approx.GVA", "Net Expenditure")
    abs <- melt(abs, id=c("Desc", "Year"), variable.name= "Type")
    abs$value <- abs$value/1e3
    
    ggplot(data = abs, aes(x = Year, y = value)) + 
      geom_bar(stat = "identity", aes_string(fill= "Desc")) +
      fte_theme()  +
      facet_grid( Type  ~ Desc) +
      scale_fill_brewer(palette = "Set2") +
      scale_y_continuous(labels = comma)+
      ylab("Value (£ Billion)") +
      ggtitle("The Annual Business Survey for Arts, Entertainment and Recreation, Information and Communication\ncovering the Non-Financial Business Economy which accounts for approx. 75% of the UK economy in terms of GVA*") +
      theme(legend.position="top") +
      theme(legend.title=element_blank()) +
      theme(legend.text = element_text(size=7, color=brewer.pal("Greys", n=9)[7])) 
    
  })
  
}) # end server

#######################################################
## knit the app
#######################################################

shinyApp(ui = ui, server = server)