sample1<- read.csv("C:/Users/leelavathi.a/Desktop/FINAL SHINY/ALL.csv")
RATINGS <- read.csv("C:/Users/leelavathi.a/Desktop/FINAL SHINY/RATINGS.csv")
measure<-read.csv("C:/Users/leelavathi.a/Desktop/FINAL SHINY/RAW_DATA.csv")
shinyServer(function(input,output,session){
  
  data<-function(place){
    Niobraradata<-subset(sample1,CONTRACT_ID == place)
    return(Niobraradata)
    
  }
  funct<-reactive({
    entity2<-data(input$Input1)
    
  })
  funct2<-reactive({
    entity3<-data(input$Input2)
    
  })
  funct3<-reactive({
    entity4<-data(input$Input3)
    
  })
  funct4<-reactive({
    entity5<-data(input$Input4)
    
  })
  
  output$contents2 <-DT::renderDataTable({
    data.frame(funct2())
  })
  output$contents3 <-DT::renderDataTable({
    data.frame(funct3())
  })
  output$contents4 <-DT::renderDataTable({
    data.frame(funct4())
  })
  

  
  output$contents5 <-renderPlot({
    barplot(funct2()$Rating,names = funct2()$Year,
            xlab = "year", ylab = "rating",
            main = "RATING")
  })
  output$contents6 <-renderPlot({
    barplot(funct3()$Rating,names = funct3()$Year,
            xlab = "year", ylab = "rating",
            main = "RATING")
  })
  output$contents7 <-renderPlot({
    barplot(funct4()$Rating,names = funct4()$Year,
            xlab = "year", ylab = "rating",
            main = "RATING")
  })

  
  
  data1<-function(place){
      RAT<-subset(RATINGS,CONTRACT_ID == place)
      return(RAT)
  }
  funct23<-reactive({
      
      data1(input$Input2)
  })
  funct24<-reactive({
      
      data1(input$Input3)
  })
  funct25<-reactive({
      
      data1(input$Input4)
  })
  fun23<-reactive({
      
      RATINGS<-funct23()[,as.integer(input$Input11)]
      #data(input$Input4)
  })
  fun24<-reactive({
      
      RATINGS<-funct24()[,as.integer(input$Input11)]
      #data(input$Input4)
  })
  fun25<-reactive({
      
      RATINGS<-funct25()[,as.integer(input$Input11)]
      #data(input$Input4)
  })
  output$phonePlot1 <- renderScatterD3({
      
      scatterD3(x = fun23() , y =fun23(), col_var = funct23()$Year , symbol_var = funct23()$Year,
                xlab = "Rating", ylab = "Rating", col_lab = "year", symbol_lab = fun23())
  }) 
  output$phonePlot2 <- renderScatterD3({
      
      scatterD3(x = fun24() , y =fun24(), col_var = funct24()$Year , symbol_var = funct24()$Year,
                xlab = "Rating", ylab = "Rating", col_lab = "year", symbol_lab = fun24())
  }) 
  output$phonePlot3 <- renderScatterD3({
      
      scatterD3(x = fun25() , y =fun25(), col_var = funct25()$Year , symbol_var = funct25()$Year,
                xlab = "Rating", ylab = "Rating", col_lab = "year", symbol_lab = fun25())
  }) 
  
  
  
  data2<-function(place){
      RAT1<-subset(measure,CONTRACT_ID == place)
      return(RAT1)
  }
  funct26<-reactive({
      
      data2(input$Input2)
  })
  funct27<-reactive({
      
      data2(input$Input3)
  })
  funct28<-reactive({
      
      data2(input$Input4)
  })
  fun26<-reactive({
      
      #meas<-funct26()[,as.integer(input$Input11)]
      a=colnames(funct26())[as.integer(input$Input11)]
      RATINGS1<-funct26()[,as.integer(input$Input11)]
      RATINGS1<-data.frame(RATINGS1)
      colnames(RATINGS1)=a
      return(RATINGS1)
      #data(input$Input4)
  })
  fun27<-reactive({
      
      meas<-funct27()[,as.integer(input$Input11)]
      #data(input$Input4)
  })
  fun28<-reactive({
      
      meas<-funct28()[,as.integer(input$Input11)]
      #data(input$Input4)
  })
  output$phonePlot4 <- renderPlotly({
    a<-fun26()
    year<-funct26()$Year
    plot_ly(a, x =  a, y  = a, text = paste("colnames:",colnames(a),
            mode = "markers", color = a, size = funct26()$Year, opacity=a))
    
    
      # scatterD3(x = fun26() , y =fun26(), col_var = funct26()$Year , symbol_var = funct26()$Year,
      #           xlab = "Rating", ylab = "Rating", col_lab = "year", symbol_lab = fun26())
  }) 
  output$phonePlot5 <- renderPlotly({
      
    plot_ly(data.frame(fun27()), x =  fun27(), y  = fun27(), text = paste("fun27()",fun27()),
            mode = "markers", color = fun27(), size = funct27()$Year, opacity =fun27())
  }) 
  output$phonePlot6 <- renderPlotly({
      
    plot_ly(data.frame(fun28()), x =  fun28(), y  = fun28(), text = paste("fun28()",fun28()),
            mode = "markers", color = fun28(), size = funct28()$Year, opacity =fun28())
  })
  # output$content <-DT::renderDataTable({
  #   data.frame(fun26())
  # })
  })
