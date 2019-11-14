library(shiny)
shinyUI(navbarPage("Fit-Appli",
    tabPanel("Calculez votre IMC",
        sidebarLayout(
            sidebarPanel(
                sliderInput(inputId="slider1",label="Taille (en cm)",min=120,max=220,value=170),
                sliderInput(inputId="slider2",label="Poids (en kg)",min=40,max=160,value=60),
                actionButton(inputId="bouton",label = "Resultat")
            ),
            mainPanel(
                h2("Mon IMC est de :"),
                h2(textOutput("IMC"))
            )
        )
    ),
    tabPanel("Interpretation selon l'OMS",
        sidebarLayout(
            titlePanel("Diagnostic :"),
            mainPanel(
                h2("Selon l'OMS, votre IMC correspond a un cas de :"),
                h2(textOutput("OMS"),style="color: red")
            )
        )
    )
))
