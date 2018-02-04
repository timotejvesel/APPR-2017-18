library(shiny)

function(input, output) {
  
  output$tabela <- DT::renderDataTable(DT::datatable({
    data <- statistika
    if (input$leto != "Vsa") {
      data <- data[data$Leto == input$leto,]
    }
     if (input$krog != "Vsi") {
      data <- data[data$Krog == input$krog,]
    }
    data
    
  
  }))
  
  output$graf1 <- renderPlot({
    tabela1 <- povprecja[c("Krog", input$sprem)]
    colnames(tabela1) <- c("Krog", "Spremenljivka")
    
    print(ggplot(tabela1) + 
            aes(x = Krog, y = Spremenljivka) + geom_col(fill = "thistle2", color = "black") + 
            scale_x_continuous(name = "Krog", breaks = seq(1,12,1)) + 
            scale_y_continuous(name = input$sprem) + 
            ggtitle(paste("Povprečna število", input$sprem, 
                          "na tekmo glede na krog izbora", sep = ' ')) 
    )
  })
    
  output$graf2 <- renderPlot({
    
    tabela2 <- stat.tekma[c("Izbor","Krog", input$sprem2)]
    colnames(tabela2) <- c("Izbor", "Krog", "Spremenljivka2")
    
    print(ggplot(tabela2) + 
            aes(x = Izbor, y = Spremenljivka2) + 
            geom_point(aes(color = factor(tabela2$Krog))) + 
            xlab("Izbor") + ylab(input$sprem2) + 
            labs(colour = "Krog") #+ 
               #ggtitle(paste("Število", input$sprem2,
                          # "na tekmo na igralca izbranega na naboru", sep = ' ')) 
          
          
    )
  })
  

}
