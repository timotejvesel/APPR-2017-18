library(shiny)

fluidPage(
  titlePanel("Statistika"),
  
  fluidRow(
    column(4,
      selectInput("leto",
                  "Leto:",
                  c("Vsa",
                    unique(statistika$Leto)))
    ),
      column(4,
             selectInput("krog",
                         "Krog:",
                         c("Vsi",
                           sort(unique((statistika$Krog)))))
      )
  ),
  fluidRow(
    DT::dataTableOutput("tabela")
  ),
  
  tabPanel("Graf",
           
           sidebarPanel(
             selectInput("sprem", label="Izberi kategorijo",
                         choices=colnames(povprecja[c(-1)]))
           ),
           mainPanel(plotOutput("graf1"))),
  
  tabPanel("Graf",
           sidebarPanel(
             selectInput("sprem2", label="Izberi kategorijo",
                         choices=colnames(stat.tekma[c(-1,-2,-3,-4)]))
           ),
           mainPanel(plotOutput("graf2")))
  
             
           )
  


