library(shiny)

shinyServer(function(input, output) {
    donnees<-reactiveValues(taille=NULL,poids=NULL,IMC=NULL)
    observeEvent(input$bouton,{
        donnees$taille<-input$slider1
        donnees$poids<-input$slider2
        donnees$IMC<-round(input$slider2/((input$slider1/100)^2),digits=2)
    })
    output$IMC <- renderText({
       donnees$IMC
    })
    output$OMS <- renderText({
        a=ifelse(donnees$IMC<18.5,"Insuffisance ponderale",ifelse(donnees$IMC<25,"Corpulence normale",ifelse(donnees$IMC<30,"Surpoids",ifelse(donnees$IMC<35,"Obesite moderee",ifelse(donnees$IMC<40,"Obesite severe","Obesite morbide")))))
        a
    })
})
