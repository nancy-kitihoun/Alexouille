library(kableExtra)
library(markdown)
library(PRROC)
library(pROC)
library(ROCR)
library(shiny)
library(shinydashboard)

library(shiny)
library(shinydashboard)



dashboardPage(
  dashboardHeader(title = "Détection de la fraude sur les cartes de crédits", titleWidth=800,dropdownMenu(type = "tasks", badgeStatus = "success",
                                                                                                           
                                                                                                           
                                                                                                           taskItem(value = 75, color = "aqua",
                                                                                                                    "Server deployment"
                                                                                                           ),
                                                                                                           taskItem(value = 80, color = "olive",
                                                                                                                    "Overall project"
                                                                                                           )
  )),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduction", tabName = "intro", icon = icon("home")),
      menuItem("Données", tabName = "datafile", icon = icon("table")),
      menuItem("Analyse", tabName = "analysis", icon = icon("chart-bar")
      ),
      menuItem("Notice", tabName = "paper", icon = icon("file-pdf-o")),
      hr(),
      sidebarUserPanel(name = a("Camille AZANGHO",
                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                       subtitle = "brunellekas@gmail.com"),br(),
      sidebarUserPanel(name = a("MAKHOKH Lamyae",
                                href = "https://www.univ-orleans.fr/deg/masters/ESA/etu/promo_M22020.html"),
                       subtitle = "lam.makhokh@gmail.com"),br(),
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
      background-color:#8fbc8f 	;
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
      
      #Introduction 
      tabItem(tabName = "paper", 
              
              box( title="Telechargement", 
                   downloadButton("notice", "Télecharger la notice de ce démonstrateur en PDF"))),
      
      tabItem(tabName = "intro", includeMarkdown("intro.md")), 
      
      
      tabItem(tabName = "datafile",
              style = "overflow-y:scroll;",
              
              # The id lets us use input$tabset1 on the server to find the current tab
              height = "250px",
              tabBox(selected = "Stat.descriptives",width = 12,
                     tabPanel("Stat.descriptives1",
                              fluidPage(
                                box(width = 12, status = "primary",
                                    title=("Aperçu des données"), 
                                    DT::dataTableOutput("dataTable")
                                ),
                                
                                box(status = "primary", 
                                    title=(" Quelques statistiques descriptives"),
                                    DT::dataTableOutput("Des1")),
                                
                                box(status = "primary",tags$footer(tags$em(
                                  "le nombre de valeurs nulles (q_zeros) et son pourcentage (p_zeros)",br(),
                                  "le nombre de valeurs manquantes (q_na) et son pourcentage (p_na)",br(),
                                  "le nombre de valeur infinies (q_inf) et son pourcentage (p_inf)",br())),
                                  
                                  DT::dataTableOutput("Des2")))),
                     tabPanel("Relations entre les variables",
                              
                              
                              
                              fluidRow(columns=12,
                                       sidebarPanel(
                                         
                                         #Definition 1er menu deroulant
                                         selectInput("variable1", "Variable (axe des x):",
                                                     list("Time" , "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                          "V9","V10","V11" , "V12","V13","V14"  , 
                                                          "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                          "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount" )),
                                         
                                         #Definition du second menu deroulant
                                         selectInput("variable2", "Variable (axe des y):",
                                                     list( "V1" ,"V2", "V3" ,"V4" , "V5" ,"V6","V7" ,"V8" ,
                                                           "V9","V10","V11" , "V12","V13","V14"  , 
                                                           "V15", "V16"   , "V17"   , "V18" ,   "V19"  ,  "V20"  ,  "V21"  ,  
                                                           "V22", "V23", "V24", "V25" , "V26", "V27"  ,  "V28" , "Amount","Time")),
                                         sliderInput("taille", "Choisir la taille des données" , min=1000, max=100000, value=500
                                         ),
                                         actionButton(inputId="graph", "Go!"),
                                         p("Cliquez le bouton pour actualiser les données.",br(),
                                           "NB:Augmentation de la taile des données augmentera le temps d'exécution ")),
                                       
                                       mainPanel( plotOutput("plot2")))),
                     tabPanel("Stat.descriptives2",
                              
                              fluidPage(
                                box( title="Fréquence de la variable cible Classe",status = "primary",  plotOutput("Grap2", height = "300px"),tags$footer(tags$em("Proportion des 0 et 1  dans la variable dépendante"))),
                                box( title="Corrélation", plotOutput("tabCor") ,status = "primary",tags$footer(tags$em("En probabilité et en statistique, 
                              la corrélation entre plusieurs variables aléatoires ou statistiques est une notion de liaison qui contredit leur indépendance.
                               Ici  nous avons des variables très peu correlées entre elles "))))
                     ),
                     tabPanel("Densité des variables", tatus= "primary",width = 24,
                              plotOutput("Grap3", width = "100%" ),tags$footer(tags$em("Les variables sont toutes centrées sur zéro "))
                              
                     ))),
      # Analyse
      tabItem(tabName="analysis",
              
              sidebarLayout(
                sidebarPanel(
                  sliderInput("prop", "Choisir la propotion des 0 dans l'echantillon d'apprentissage" , min=0.01, max=1, value=0.7
                  ),
                  actionButton(inputId="proportion", "Go!"),
                  p("Cliquez le bouton pour actualiser les données .",br(),
                    "NB:",br(),"- L'Augmentation de la taile des données augmentera le temps d'exécution. ",
                    br(),
                    "-Plus on tend vers 1 moins il y a de zéros dans l'échantillon d'apprentissage")),
                
                
                mainPanel(
                  tabBox(selected = "SVM",width = 12,
                         
                         tabPanel("SVM", 
                                  fluidPage(
                                    box(width = "100%", verbatimTextOutput("svm1"),status = "primary", title="Sortie SVM", tags$footer(tags$em("En
                                    fonction de la valeur mise par défaut on constate un taux d'exactitude de 0.9195 "))),
                                    box(width="100%",verbatimTextOutput("performence"),status = "primary",title="Matrice de confusion")),
                                  
                                  
                                  
                                  
                         ),
                         tabPanel("Arbre de classification",
                                  fluidPage(
                                    box(width = "100%", verbatimTextOutput("tree"),status = "primary", title="Sortie arbre de classification",tags$footer(tags$em("Misclassification error rate est ici 
                                                    le taux d'erreur d'apprentissage. Comme nous pouvons le voir il est faible "))),
                                    box(width = "100%", plotOutput("arbre", height = "300px" ),status = "primary"),
                                    box(width = "100%", verbatimTextOutput("treroc"),status = "primary")
                                    
                                    
                                  )),
                         tabPanel("Gradient boosting",
                                  fluidPage(
                                    box(width = 6, plotOutput("gradient"),status = "primary", title="Estimation avec le gradient boosting",tags$footer(tags$em("Le graphe
                                    ci-dessus nous affiche les variables en fonction de leur degré d'importance.
                                    La performance est également très satisfaisante : taux d'exactitude de 0.9215
                            
                                                                                                                "))),
                                    box(width = 6, verbatimTextOutput("perfgradient" ),status = "primary"))
                                  
                         ),
                         tabPanel("Logistique",
                                  fluidPage(
                                    box(width = "100%", verbatimTextOutput("logs"),status = "primary", title="Estimation avec la logistique"),
                                    box(width = "100%", verbatimTextOutput("perflog" ),status = "primary",title="Performance")
                                    
                                    
                                  )),tabPanel("Comparaison des modèles",
                                              fluidPage(
                                                
                                                plotOutput("roc")
                                              ))
                         
                         
                         
                         
                         
                         
                  ))))
    ))
  
  
  
)