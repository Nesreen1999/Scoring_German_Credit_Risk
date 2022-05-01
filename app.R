#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plotly)
library(shiny)
library(bslib)
library(dplyr)
library(caret)
library(ROCR)
library(pROC)
library(aod)
library(fastDummies)
library(glue)
library(DT)
library(tidyverse)
library(readxl)
library(data.table)
library(e1071)
df <- read_excel("C:/Users/Nessrine/Desktop/german credit dataset_Projet.xlsx")
df$`Solde du compte courant`<- as.factor(df$`Solde du compte courant`)
df$`Historique des credits`<-as.factor(df$`Historique des credits`)
df$`Compte d'epargne / obligations`<-as.factor(df$`Compte d'epargne / obligations`)
df$`Employe depuis`<- as.factor(df$`Employe depuis`)
df$Motif<- as.factor(df$Motif)
df$`Statut personnel et sexe`<- as.factor(df$`Statut personnel et sexe`)
df$`Autres debiteurs / garants`<- as.factor(df$`Autres debiteurs / garants`)
df$`Residence actuelle depuis`<- as.factor(df$`Residence actuelle depuis`)

df$Logement<-as.factor(df$Logement)
df$`Nombre de credits existants dans cette banque`<-as.factor(df$`Nombre de credits existants dans cette banque`)
df$Emploi<- as.factor(df$Emploi)
df$`Nombre de personnes à charge`<- as.factor(df$`Nombre de personnes à charge`)
#Remplacer le statut en 0=Non solvable et 1= Solvable pour 
#sa reconnaissance dans le modele apres en tant que variable dummy
df$Statut<-ifelse(df$Statut==2,0,1)
df$Statut<- as.factor(df$Statut)
df$Age<-cut(df$Age,breaks=c(15,30,50,70,92),
            right = FALSE,include.lowest=T,
            labels =c("15-30","30-50","50-70","70-92"))
df$`Duree de credit (en mois) factor`<- cut(df$`Duree de credit (en mois)`,breaks = c(0,10,20,30,40,60),
                                            right = FALSE,include.lowest=T,
                                            labels =c("moins de 10","10-20","20-30","30-40","40 et plus"))
df$`Montant du credit factor`<- cut(df$`Montant du credit`,breaks = c(0,1000,5000,10000,16000),
                                    right = FALSE,include.lowest=T,
                                    labels = c("moins de 1000","1000-5000","5000-10000","plus de 10000"))
colnames(df)<-c("solde","Credit_term","Historic","Motif","Amount","Savings_and_Bonds","Employer","Sex_status","Other","Residence","Age","lodging","Existing_credit","Job","Nbr_persons_in_charge","Statut","Credit_term_factor","Credit_amount_factor")
attach(df)
df=na.omit(df)
quali<-c("Age","Motif","Credit_amount_factor","Credit_term_factor","Nbr_persons_in_charge","Job","Existing_credit","Lodging","Residence","Other","Sex_status" ,"Employer","Savings_and_Bonds","Historic","Solde" )
quanti<-c("Amount","Credit_term")
data2<-dummy_cols(df,
                  select_columns = c("solde","Historic","Motif","Savings_and_Bonds","Employer","Sex_status","Other","Residence","Age","lodging","Existing_credit","Job","Nbr_persons_in_charge","Credit_term_factor","Credit_amount_factor")
                  ,remove_most_frequent_dummy = T,
                  remove_selected_columns = T)
set.seed(421)
rows<-sample(nrow(data2))
data2<-data2[rows,]
#Train and test set:
#split<-round(nrow(data2)*0.60)
split<-createDataPartition(data2$Statut,list = FALSE,p=0.80)
train<-data2[split,]
test<-data2[-split,]
#fit a logistic regression model:
model<-glm(Statut~.,family = "binomial",train)

# Intervalle de confiance des parametres
ci=confint(model)
wt<-wald.test(b = coef(model), Sigma = vcov(model), Terms = 1:17)

theme <- bs_theme(
    bootswatch = "flatly"
)


# Define UI for application that draws a histogram
if(interactive()){
ui <- fluidPage(
    
    navbarPage("Credit Risk Dashboard",
               tabPanel("About",sidebarLayout(
                   sidebarPanel(img(src = "aznes.jpg",width=280,height=380, align = "center")),
                   mainPanel(
                       tabsetPanel(tabPanel("About this project :",br(),"@authors:",br(),"Nesrine Fekih-Romdhane & Azza Cherni" ,br(),br(),"As part of an econometrics project at ESSAI, we aim to realize an univariate and bivariate analysis of the (German Credit Dataset) database as well as a predictive model of the train database using several methods.",br(),"The German Credit Data contains data on 20 variables and the classification whether an applicant is considered a Good or a Bad credit risk for 1000 loan applicants. A predictive model developed on this data is expected to provide a bank manager guidance for making a decision whether to approve a loan to a prospective applicant based on his/her profiles."),
                                   tabPanel("Contacts" ,br(), uiOutput("tab"),uiOutput("tab1"),br(),"Your suggestions, comments, complaints or compliments are highly appreciated and will guide us in
                                            continuously improve the dashboard."))
                       
                   )
               )
                          ),
               tabPanel("Data",sidebarLayout(
                   sidebarPanel(
                       img(src = "data.jpg",width=280,height=200, align = "center"),
                       radioButtons("rd1","Display :",c("Preview","Summary"))
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                       tabsetPanel(
                           tabPanel("View",DT::dataTableOutput("data")),
                           tabPanel("Description",verbatimTextOutput("des"))
                       )
                       
                   )
               )),
               navbarMenu("Statistics",
                          tabPanel("Univariante",sidebarLayout(
                              sidebarPanel(
                                  h5("Univariante statistics"),
                                  img(src = "stat.jpg",width=280,height=200, align = "center"),
                                  selectInput("sel"," qualitative X-variable :",quali),
                                  selectInput("sel1","quantitative variable",quanti)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(h4("Barplot"),plotlyOutput("bar"),h4("Pie chart"),
                                        plotlyOutput("pie"),h4("Density"),
                                        plotlyOutput("densite")
                                        )
                          )
                                   ),
                          tabPanel("Bivariante",sidebarLayout(
                              sidebarPanel(
                                  h5("Bivariante statistics"),
                                  img(src = "stat.jpg",width=280,height=200, align = "center"),
                                  selectInput("sel2"," qualitative variable :",quali),
                                  selectInput("sel3","quantitative variable",quanti)
                              ),
                              
                              # Show a plot of the generated distribution
                              mainPanel(h4("Boxplot"),plotlyOutput("box"),
                                        h4("Student test"),
                                        tableOutput("test")                              )
                          )
                                   )),
               navbarMenu("Model",
                          tabPanel("Logistic regression (GLM)",sidebarLayout(
                              sidebarPanel(h5("Logistic regression (GLM)"),img(src = "glm.png",width=280,height=200, align = "center"),"Response variable : Statut",br(),"Train set : 80%"),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Predict",verbatimTextOutput("p")),
                                      tabPanel("Confidence interval",verbatimTextOutput("ci")),
                                      tabPanel("Wald test",verbatimTextOutput("wt")),
                                      tabPanel("Evaluation table",tableOutput("ET")),
                                      tabPanel("ROC Curve",plotOutput("roc"))
                                  )
                              )
                          )),
                          tabPanel("Support Vector Machine (SVM)",sidebarLayout(
                              sidebarPanel(h5("Support Vector Machine (SVM)"),img(src = "svm.png",width=280,height=200, align = "center"),"Response variable : Statut",br(),"Train set : 80%"),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Predict",verbatimTextOutput("p2")),
                                      tabPanel("Confusion Matrix for test set",verbatimTextOutput("cm3")),
                                      tabPanel("ROC Curve",plotOutput("roc2"))
                                  )
                              )
                          )),
                          tabPanel("Random Forest",sidebarLayout(
                              sidebarPanel(h5("Random Forest"),img(src = "rf.png",width=280,height=200, align = "center"),"Response variable : Status",br(),"Train set : 80%"),
                              mainPanel(
                                  tabsetPanel(
                                      tabPanel("Predict",verbatimTextOutput("p1")),
                                      tabPanel("Confusion Matrix for test set",verbatimTextOutput("cm2")),
                                      tabPanel("ROC Curve",plotOutput("roc1"))
                                  )
                              )
                          ))),
               tabPanel("Test",sidebarLayout(
                   sidebarPanel(h4("Enter your informations :"),
                                   selectInput("solde","Current account balance ",c("< 0","0<=..<200",">=200","No current account")),
                                   selectInput("duree","Loan duration in months",c("..<10","10-20","20-30","30-40","40<..")),
                                   selectInput("historic","Credit history",c("no credit taken / all credits refunded","all credits of this bank have been repaid","credits already repaid so far","late payment in the past","critical account / other existing credits (not in this bank)")),
                                   selectInput("Motif","Motif",c("new car","used car","supply/equipment","Television","Appliance","repair","Education","Recycling (retraining)","Project","Other")),
                                   selectInput("montant","Credit amount",c("..<1000","1000-5000","5000-10000","10000<..")),
                                   selectInput("epargne","Savings Account/Bond",c("..<100","100-500","500-1000","1000<..","No savings account")),
                                   selectInput("occupation","Currently occupied since ",c("Unemployed","..<1 year","1<=..<4 years","4<=..<7 years","7<..")),
                                   selectInput("sexe","Personal status and gender",c("divorced man","divorced/married women","single man","married man","single woman")),
                                   selectInput("other","Other debtors / guarantors",c("No","co-applicant","Guarantor")),
                                   selectInput("residence","Current residence since",c()),
                                   selectInput("age","Age",c("15-30","30-50","50-70","70-92")),
                                   selectInput("lodging","Lodging",c("occupant","owner","free of charge")),
                                   selectInput("nbrcred","Number of current loans in this bank",c()),
                                   selectInput("job","Job",c("unemployed / unskilled - non-resident","unqualified - resident","Employee/Official","Manager/Freelancer/Skilled Worker")),
                                   selectInput("nbrper","Number of persons in charge",c())
                                   
                                   
               ),
               mainPanel ()
               
               )),
     
               theme = theme
               
    )
)
# Define server logic required to draw a histogram
server <- function(session,input, output) {
    output$tab<-renderUI({
        url <- a("Nesrine Fekih Romdhane", href="linkedin.com/in/nesrine-fekih-romdhane/")
        url2 <- a("Azza Cherni", href="linkedin.com/in/azza-cherni-34900b1bb/")
        tagList("Check our linkedIn pages :",br(),  url ,br(), url2)
    })
    output$tab1<-renderUI({
        url <- a("nesrine.fekihromdhane@gmail.com ", href="nesrine.fekihromdhane@gmail.com")
        url2 <- a("azza.cherni@outlook.com", href="azza.cherni@outlook.com")
        tagList("E-mails :",br(), url ,br(),url2)
    })
    output$des<-renderPrint({str(df)})
    output$data<-DT::renderDataTable(
        switch (input$rd1,
            Preview = df ,
            Summary = summary(df))
        )
    
    output$densite<-renderPlotly({
        x = unlist(df[,input$sel1])
        fit <- density(x)
        plot_ly(x = x, type = "histogram", name = "Histogram") %>% 
            add_trace(x = fit$x, y = fit$y,type='scatter' ,mode = "lines", fill = "tozeroy", yaxis = "y2", name = "Density") %>% 
            layout(title=names(df[,input$sel1]),yaxis2 = list(overlaying = "y", side = "right"))
    })

    output$bar<-renderPlotly({
        plot_ly(
            y = c(table(df[,input$sel])),
            type = "bar",
            marker = list(color = "green")
        )%>%layout(title = names(df[,input$sel]))
        })
    output$pie<-renderPlotly({
        plot_ly(
            values = c(table(df[,input$sel])),
            type = "pie",
            marker = list(color = "green")
        )%>%layout(title = names(df[,input$sel]))
    })
    #bivariersel3boxtest
    output$box<-renderPlotly({
        x = unlist(df[,input$sel3])
        p <- plot_ly(y = ~x,x= ~df$Statut , type = "box",line = list(color = "green"))%>%layout(title = names(df[,input$sel3]))
    })
    output$test<-renderTable({
        x1 <- unlist(df[,input$sel3])
        unlist(t.test(x1,df$Statut))
    })
    
    
    
    
    
    ##logit
    output$ci<-renderPrint(ci)
    output$wt<-renderPrint({wt})
    p<-predict(model,test,type="response")
    rocobj <- roc(test$Statut, p)
    auc <- round(auc(test$Statut, p),4)
    output$roc<-renderPlot( ggroc(rocobj, colour = 'pink', size = 1.5) +
                               ggtitle(paste0('ROC Curve for test set ', '(AUC = ', auc, ')')))
    Evaluation_table<-ifelse(p>0.50,"Yes","No")
    et<-table(Evaluation_table,test[["Statut"]])
    output$ET<-renderTable(et)
    output$p<-renderPrint(summary(p))
    or=exp(coef(model))
    output$od<-renderPrint({or})
    ##svm:
    classifier = svm(formula = Statut ~ .,
                     data = train,
                     type = 'C-classification',
                     kernel = 'linear')
    y_pred = predict(classifier, test,type="class")
    output$p2<-renderPrint({summary(y_pred)})
    t<-confusionMatrix(train$Statut, predict(classifier))
    output$cm3<-renderPrint({t})
    rocobj2 <- roc(test$Statut, as.numeric(y_pred))
    auc2 <- round(auc(test$Statut, as.numeric(y_pred)),4)
    output$roc2<-renderPlot(ggroc(rocobj2, colour = 'pink', size = 1.5) +
                                ggtitle(paste0('ROC Curve ', '(AUC = ', auc2, ')')))
   
    #randomforest:
    library(randomForest)
    split<-createDataPartition(df$Statut,list = FALSE,p=0.80)
    train<-df[split,]
    test<-df[-split,]
    rf<-randomForest(Statut~.,train,ntree = 600)
    pred <- predict(rf, test)
    output$p1<-renderPrint({summary(pred)})
    rocobj1 <- roc(test$Statut, as.numeric(pred))
    auc1 <- round(auc(test$Statut, as.numeric(pred)),4)
    output$roc1<-renderPlot(ggroc(rocobj1, colour = 'pink', size = 1.5) +
                                ggtitle(paste0('ROC Curve for test set', '(AUC = ', auc1, ')')))
    cc<-confusionMatrix(pred, test$Statut)
    output$cm2<-renderPrint({cc})
    ##TEST
    
    
    
     }
}

# Run the application 
shinyApp(ui = ui, server = server)