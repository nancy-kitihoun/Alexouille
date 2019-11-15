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
library(PRROC)
library(pROC)


options(shiny.maxRequestSize=150*1024^2)


undersampling = function(data, p, position)
{
    summary = table(data[,position])
    
    m_event_value = names(summary)[which.max(table(data[,position]))]
    
    m_event_index = which(data[,position]==m_event_value)
    
    m_event_length = length(m_event_index)
    
    data_length = nrow(data)
    
    new_m_event_length = floor(data_length - (data_length-m_event_length)/p)
    
    new_index = sample(m_event_index,size=new_m_event_length)
    
    new_data = data[-new_index,]
    
    new_data
    
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
    
    #data=reactive({
    # read.table("D:/M2/SVM/creditcard.csv", header =T ,sep = ",")
    # })
    data=reactive({
        readRDS(file='D:/Github/Alexouille/creditcard.rds')
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
})











