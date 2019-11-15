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

options(shiny.maxRequestSize=150*1024^2)


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

tabl=function(data,b){
    
    set.seed(123)
    attach(data)
    da1=data[Class==1,]
    
    dao=data[Class==0,]
    no=nrow(dao)
    
    p=which(Class==0)
    
    ind=sample(p, no*b)
    part=data[ind,]
    
    new=rbind(part,da1)
    neawdat=sample(1:nrow(new),nrow(new))
    newdata=new[neawdat,]
    attach(newdata)
    newdata
}

# b pouvent aller de 0,001 à 0.005

# ind=sample(2, nrow(data()), replace=TRUE ,prob=c(0.7, 0.3))
# 
# train=data[ind==1,]
# test=data[ind==2,]
# 
# 
# und=reactive({
#   s=ovun.sample(Class~., data=train(), method="under", N=1000)
#   s$data
#   
# })  

shinyServer(function(input, output,session) {
    
    #importation 
    
    #saveRDS(data,file='C:/Users/camille/Desktop/Documents/GitHub/Application/genre/creditcard.rds')
    
    data=reactive({
        readRDS(file='D:/Github/Appli/genre/genre/creditcard.rds')
    })
    
    
    
})
set.seed(1234)

output$dataTable = DT::renderDataTable( {
    datatable(data(), extensions = 'FixedColumns',editable = 'cell',
              rownames = F, options = list(pageLength = 5, dom = 'tp',scrollX = TRUE,
                                           fixedColumns = TRUE),
              selection = list(mode = "single",
                               target = "column", selected = 4)
    )
})











