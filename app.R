
library(shiny)
#detach(package:tm, unload=TRUE)
library(shinydashboard)
library(arules)


data(Groceries)
# cc=as( Groceries,"list" )
# cda = data.frame()
# for(i in 1:length(cc))
# {
#   for(j in 1:length(cc[[i]]))
#   {
#     cda = rbind(cda,data.frame(Customer = paste(c("C", i), collapse = " "), Item = cc[[i]][j]))
# 
# 
#   }
# 
# }









ui <- dashboardPage(
  dashboardHeader(title = "Market basket analysis"),
  
  dashboardSidebar(
    sidebarMenu(
    #menuItem("ECI", tabName = "ECI", icon = icon("dashboard")),
    menuItem("Grocery",tabName = "Grocery", icon = icon("dashboard"))
    #menuItem()
    )
    #radioButtons("ll","LHS or RHS",choices = c("LHS","RHS")),
    #uiOutput("sdat")
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Grocery",
              h2("Grocery") , tabsetPanel(
                tabPanel("Association rules",
                         DT::dataTableOutput("dda")  
                )
                #tabPanel("Data_for_association_rule", DT::dataTableOutput("dra")),
                #tabPanel("Raw Data", DT::dataTableOutput("drawa"))
              )
              
      )
    )
  )
)


server <- shinyServer(function(input, output) {


   
  output$dd <- DT::renderDataTable({
    rules1<-apriori(data=trans4, parameter=list(supp=0.001,conf = 0.2, minlen=2), control = list(verbose = FALSE))
    if(length(rules1)!= 0 )
    {
      ruledf = data.frame(lhs = labels(lhs(rules1)),
                          rhs = labels(rhs(rules1)),
                          rules1@quality)
    }
    DT::datatable(ruledf, filter = 'top',options = list(scrollX = TRUE))
    
     
  })
  
  output$dr <- DT::renderDataTable({
    DT::datatable(fadf, filter = 'top',options = list(scrollX = TRUE))
    
  })
  
  output$draw <- DT::renderDataTable({
    DT::datatable(fadf_raw, filter = 'top',options = list(scrollX = TRUE))
    
  })
  
  # output$drawa <- DT::renderDataTable({
  #   DT::datatable(cda, filter = 'top',options = list(scrollX = TRUE))
  #   
  # })
  
  output$dda <- DT::renderDataTable({
    rules1<-apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.1, minlen=2), control = list(verbose = FALSE))
    if(length(rules1)!= 0 )
    {
      ruledf = data.frame(lhs = labels(lhs(rules1)),
                          rhs = labels(rhs(rules1)),
                          rules1@quality)
    }
    DT::datatable(ruledf, filter = 'top',options = list(scrollX = TRUE))
    
    
  })
  
})

 
shinyApp(ui = ui, server = server)
#runApp('GitHub/shinyTime/affinity',host = "0.0.0.0",port = 5051)

