
library(kableExtra)
library(markdown)
library(shiny)
library(shinydashboard)
library(DT)
library(reshape)
library(funModeling)
library(GGally)
library(dplyr)
library(datasets)
library(ggplot2)
library(e1071)
library(randomForest)
library(tree)
library(caret)
library(parallelSVM)
library(gbm)
library(ggvis)
library(ROCR)
library(PRROC)
library(pROC)

options(shiny.maxRequestSize=1500*1024^2)


under = function(data, pro)
{
    Table = table(data[,31])
    
    nombr_cible = names(Table)[which.max(table(data[,31]))]
    
    pos_index = which(data[,31]==nombr_cible)
    
    length_nomb = length(pos_index )
    
    taille = nrow(data)
    
    nouv = floor(taille - (taille-length_nomb)/pro)
    
    pos_plus = sample(pos_index,size=nouv)
    
    nouvdata = data[-pos_plus,]
    
    nouvdata
    
}

plot_num_density <- function (data, path_out = NA) 
{
    wide_data = suppressMessages(melt(data))
    p = ggplot(data = wide_data,  aes(x = value)) + 
        geom_density( na.rm = T) + 
        facet_wrap(~variable, 
                   scales = "free") + aes(fill = variable) + guides(fill = FALSE)
    if (!is.na(path_out)) {
        export_plot(p, path_out, "density")
    }
    plot(p)
}



shinyServer(function(input, output,session) {
   
    output$notice= downloadHandler(
        filename = function() {
            paste('test-PDF-', format(Sys.Date(), "%d-%m-%Y"), '.pdf', sep='')
        },
        content = function(con) {
            fileRmd   <- paste0("genre/", "Notice.Rmd")
            fileTex   <- paste("genre/pdfTest_", Sys.Date(), ".tex", sep = "")
            folderTex <- paste0("genre/pdfTest_", Sys.Date(), "_files")
            
            try(detach("package:kableExtra", unload = TRUE))
            try(detach("package:rmarkdown", unload = TRUE))
            
            options(kableExtra.latex.load_packages = FALSE)
            library(kableExtra)
            library(rmarkdown)
            
            try(rmarkdown::render(fileRmd,
                                  pdf_document(latex_engine = "pdflatex"),
                                  output_file = con,
                                  encoding = "UTF-8",
                                  clean = TRUE))
            
            unlink(folderTex, recursive = T) })
        
    
    
    #data=reactive({
    # read.table("D:/M2/SVM/creditcard.csv", header =T ,sep = ",")
    # })
    data=reactive({
        readRDS(file='genre/creditcard.rds')
    })
    
    set.seed(1234)
    
    output$dataTable = DT::renderDataTable( {Sys.sleep(1)
        datatable(data(), extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4)
        )
    })
    output$Des1 =DT::renderDataTable({ 
        datatable(profiling_num(data()),extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 4, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4) )})
    
    output$Des2 =DT::renderDataTable({ 
        datatable(df_status(data()),extensions = 'FixedColumns',editable = 'cell',
                  rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                               fixedColumns = TRUE),
                  selection = list(mode = "single",
                                   target = "column", selected = 4) )})
    
    selectedData <- reactive({
        data()[1:input$taille, c(input$variable1, input$variable2)]
        
    })
    #grade de liaison entre les variables.
    B1=eventReactive(input$graph, {selectedData()})
    
    output$plot2 <- renderPlot({
        
        #par(mar = c(5.1, 4.1, 0, 1))
        plot(B1(),col="#70678a")
        
    })
    
    
    #height = 400
    # width = 600})
    output$Grap2<- renderPlot({freq(data()[,31])})
    output$tabCor <-renderPlot({ ggcorr(data()[,-31])})
    
    # pour une page qui affiche tout un graph complet 
    
    
    
    output$Grap3<- renderPlot({plot_num_density(data()[,c(-31)]) 
        height = 600
        width = 800 })
    
    #------------------------------------------------ partie analyse
    
    # svm
    inde=reactive({set.seed(123456)
        sample(1:nrow(data()),nrow(data())*0.7)     })
    
    
    
    train=reactive({
        
        x = data()[inde(),]
        x$Class = as.factor(x$Class)
        x
    })
    
    
    test=reactive({
        x = data()[-inde(),]
        x$Class = as.factor(x$Class)
        x
    })
    
    b=reactive({input$prop} )
    
    appren=reactive({
        under(train(),b())
    })
    
    
    svm_result=reactive({  
        
        svm(Class ~., data = appren(),
            kernel = "linear",
            type = "C-classification",probability=TRUE)
    })
    A1=eventReactive(input$proportion, {svm_result()})
    output$svm1=renderPrint(A1())
    
    
    a=reactive({
        predict(A1(),test()[,1:30],probability=TRUE)
    })
    #predicte()
    output$performence =renderPrint({confusionMatrix(predict(A1(),newdata=test(),probability=TRUE),test()$Class,positive="1")
    })
    q= reactive({
        p= roc.curve(a(),as.factor(test()$Class),curve = TRUE,max.compute = TRUE, 
                     min.compute = TRUE, rand.compute = TRUE)
        plot(p,max.plot = TRUE, min.plot = TRUE, rand.plot = TRUE, fill.area = TRUE)})
    output$ROC=renderPlot({q()})
    
    #abre de classification
    tre=reactive({
        tree(Class~., appren())})
    A2=eventReactive(input$proportion, {tre()})
    
    output$tree=renderPrint({ Sys.sleep(1);
        summary(A2())})
    
    output$arbre= renderPlot({plot(A2())
        text(A2(), pretty=0)    })
    
    
    
    output$treroc=renderPrint({ 
        
        tree.pred=predict(A2(),newdata=test(),type="class")
        
        confusionMatrix(as.factor(tree.pred),test()[,31],positive="1")
        
    })
    
    
    
    # gradient boosting
    boost=reactive({
        
        g=gbm(as.character(Class)~., data=appren(), distribution="bernoulli", n.trees= 500, interaction=4)
        g})
    A3=eventReactive(input$proportion, {boost()})
    output$gradient=renderPlot(summary(A3()))
    
    output$perfgradient=renderPrint({
        
        pred.boost=predict(A3(), newdata=test(), n.trees=500,type ='response',interaction=4) 
        
        class_pred = ifelse(pred.boost<0.5, "0", "1")   
        confusionMatrix(as.factor(class_pred), test()$Class, positive = "1")
    })
    
    #logistique 
    logis=reactive({
        glm.fit2=glm(Class~.,data=appren(),family=binomial)})
    A4=eventReactive(input$proportion, {logis()})
    output$logs=renderPrint({summary(A4())})
    
    
    
    confus=reactive({
        glm.probs=predict(A4(),test(), type="response")
        glm.pred=rep(0,nrow(test()))
        glm.pred[glm.probs>.5]=1
        confusionMatrix(as.factor(glm.pred), test()$Class, positive = "1")
    })
    output$perflog=renderPrint({confus()})
    
    #-------------------------Courbe ROC
    
    
    # svm marche
    rocc=eventReactive(input$proportion,{
        rocsvmq=attributes(a())$probabilities[,2]
        pred=prediction(rocsvmq,test()$Class)
        per=performance(pred,"tpr","fpr")
        auc_vm=performance(pred,"auc")
        auc_vm=unlist(slot(auc_vm,"y.values"))
        eq = paste0("Auc_svm = ", round(auc_vm,4))
        #+++++++++++++++++++++++++++++++++++++++++++++++++++++
        #logis: arbre ne marche pas
        
        #roc1=eventReactive(input$proportion,
        #{
        # tree.pre=predict(A2(),newdata=test(),type = "class")[,2]
        
        # pred1=prediction(tree.pre,test()$Class)
        # as.numeric(performance(pred1, "tpr","fpr")@y.values)
        
        #plot(performance(pred1,"tpr","fpr"),color=T)
        
        #++++++++++++++++++++++++++++++++++++++++++++++++                
        #logisti: marche
        
        pree=predict(A4(),test(), type="response")
        pre=prediction(pree,test()$Class)
        preee=performance(pre,"tpr","fpr")
        auc_logis=performance(pre,"auc")
        auc_logis=unlist(slot(auc_logis,"y.values"))
        eq2 = paste0("Auc_logis = ", round(auc_logis,4))
        
        #+++++++++++++++++++++++++++++++++++++++++++++++++
        # pour le gradient boos marche
        
        pred.boo=predict(A3(), newdata=test(), n.trees=500,type ='response',interaction=4)
        preboo=prediction(pred.boo,test()$Class)
        preboos=performance(preboo,"tpr","fpr")
        
        auc_gradBoost=performance(preboo,"auc")
        auc_gradBoost=unlist(slot(auc_gradBoost,"y.values"))
        eq3 = paste0("Auc_gradBoost = ", round(auc_gradBoost,4))
        
        #++++++++++++++++++++++++++++++++++++++++++++++++  
        
        plot(per,col="blue" ,main=c(eq,eq2,eq3),xlab=("1-specificity"),ylab=("sensitivity"), print.auc=T)
        plot(preee,col="green",add=T)
        plot(preboos,col="red",add=T)
        abline(a=0,b=1)
        legend( "bottomright", legend=c("svm: bleu","logis: vert","gradBoost: rouge"),
                col=c("blue","green","red"),box.lty=2, box.lwd=2)
    })
    
    
    
    output$roc=renderPlot({rocc()})
    
    
})
    

    
    

