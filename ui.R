library(shiny)
library(shinydashboard)



dashboardPage(title = "Détection des fraudes sur les cartes de crédits",
              skin = ("green"),
              dashboardHeader(title = "Détection des fraudes sur les cartes de crédits", titleWidth=800),
              
              dashboardSidebar(
                  sidebarMenu(
                      menuItem("Introduction", tabName = "intro", icon = icon("home")),
                      menuItem("Données", tabName = "datafile", icon = icon("table")),
                      menuItem("Analyse", tabName = "analysis", icon = icon("chart-bar")
                      ),
                      menuItem("Paper", tabName = "paper", icon = icon("file-pdf-o")),
                      hr(),
                      sidebarUserPanel(name = a("Camille AZANGHO",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "brunellekas@gmail.com"),br(),
                      sidebarUserPanel(name = a("MAKHOKH Lamyae",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "lamayae1993makhokh@gmail.com"),br(),
                      sidebarUserPanel(name = a("KITIHOUN Nancy",
                                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                                       subtitle = "nancy.kitihoun@yahoo.com")
                      
                      
                      
                      
                  ),
                  tags$head(tags$style(HTML(
                      '/*header title font*/
.main-header .logo{
  font-family: "Geogia",Times,"Times New Roman",Serif;
  font-weight:  bold;
  font-size: 24px;
}

/*background color of hearder (logo part)*/
.skin-blue  .main-header .logo{
  background-color: #8a6777;
}

/* change the background color of hearder (logo part) when mouse hover */
 .skin-blue .main-header .logo:hover {
   background-color:orange;
 }
 
 /*background color for remaining part of the header */
 .skin-blue .main-header .navbar {
   background-color: #67818a;
 }
 
 /* main sidebar */
   .skin-blue .main-sidebar {
           background-color:#67818a;
   }
 
 
 /* active sidebar menu item */
 
    .skin-blue .main-sidebar .sidebar-menu .active a{
      background-color: white;
      color:black;
      
    }
    /*  sidebar menuitems */
        .skin-blue .main-sidebar .sider .sidebar-menu a{
                   background-color: orange;
                          color:white;
      
        }
    /* sider menuitems when mouse hover */
    
     .skin-blue .main-sidebar  .sidebar .sidebar-menu a:hover{
        background-color: brown;
        color:white;
        
     }
     /* sidebar toggle button */
     .skin-blue .main-header  .navbar .sidebar-toggle:hover {
       background-color:black;
     }
        
                              ')))
                  
              ),
              dashboardBody(
                  
                  
                  tabItems(
                      # Read data
                      tabItem(tabName = "intro", includeMarkdown("Intro.rmd")),    
                      
                      
                      
                      tabItem(tabName = "datafile",
                              style = "overflow-y:scroll;",
                              
                              # The id lets us use input$tabset1 on the server to find the current tab
                              height = "250px",
                              tabBox(selected = "Description1",width = 12,
                                     tabPanel("Description1",
                                              box(width = 12, status = "primary",
                                                  title = "detection de fraude sur des cartes bancaires", 
                                                  DT::dataTableOutput("dataTable")
                                              )
                                     )
                              )
                      )
                  )
              )
)


