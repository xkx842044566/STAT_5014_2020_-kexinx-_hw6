library(shiny)
#library(lubridate)
library(dplyr)
library(ggplot2)

shinyServer(function(input, output) {
  
  githubURL<-("E:/vt stat/stat 5014 stat progr packages/STAT_5014_2020_-kexinx-_hw6/data/StockPrice.RDS")
  data<-readRDS(githubURL)
  
  weight<-reactive({
    
    weight<-data.frame(symbol=character(),weights=numeric())
    
    if(!is.null(input$var1)){
      weight<-weight%>%add_row(symbol='AAPL',weights=input$var1)
    }
    if(!is.null(input$var2)){
      weight<-weight%>%add_row(symbol='FB',weights=input$var2)
    }
    if(!is.null(input$var3)){
      weight<-weight%>%add_row(symbol='JPM',weights=input$var3)
    }
    if(!is.null(input$var4)){
      weight<-weight%>%add_row(symbol='MRNA',weights=input$var4)
    }
    if(!is.null(input$var5)){
      weight<-weight%>%add_row(symbol='WMT',weights=input$var5)
    }
    weight$weights<-weight$weights/sum(weight$weights)
    return(weight)
  })
  
  reactive_data <- reactive({
   data %>%
      filter(symbol %in% input$symbol) %>%
      filter(date >= input$dateRange[1] & date<= input$dateRange[2]) %>%
      left_join(weight(),by = "symbol")
  }) 
  
  
  output$timeseries <- renderPlot({
    
    ggplot(reactive_data(),aes(x=date,y=adjusted,color=symbol))+
      geom_point()+geom_line()+xlab('Date')+ylab('Adjusted price')+theme_bw()+
      ggtitle("Adjusted price over time")
    
  })
    
    
  output$piechart<- renderPlot({
    
    ggplot(reactive_data(),aes(x='',y=weights,fill=symbol))+geom_bar(width=1,stat='identity')+
      coord_polar("y",start = 0)+ggtitle("Pie Chart of Investment Weight")
    
  })
  
  output$portfolio <- renderPlot({
    
    portfoliodata<-reactive_data()%>%
      mutate(returns=adjusted/lag(adjusted,1)-1) %>%
      mutate(weighted.returns=returns*weights) %>%
      filter(date>min(date))
    
    ggplot(portfoliodata,aes(x=date,y=weighted.returns,fill=symbol))+
      geom_bar(stat='identity')
  })
  
})