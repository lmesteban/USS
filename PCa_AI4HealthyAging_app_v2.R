library(shiny)
library(bslib)
library(pROC)
library(rsconnect)
library(xgboost)

ui<-page_fluid(
  
  # Application title
  titlePanel("Prostate cancer: AI4HealthyAging clinical risk calculator"),
  layout_columns(
  col_width = c(2,2,2,2,2,2),
  
  # Sidebar with a slider input for the number of bins
  #navbarPage("WHO = 2-3-4-5 Predictive model",
  #sidebarLayout(
  # navbarMenu("Nomogram",
  #tabPanel("Nomogram + Clinical utility curve",inputPanel(
   numericInput("Age", "Age:",
                 min = 45,
                 max = 95,
                 value = 65),
    numericInput("PSA", "PSA:",
                 min = 0.00,
                 max = 200.00,
                 step=0.01,
                 value = 3,width="100%"),
    numericInput("PSA_4_10", "Previous PSA in the interval [3,10] (n):",
                 min = 0,
                 max = 50,
                 step=1,
                 value = 0),
    numericInput("PSA_10", "Previous PSA values greater than 10 (n):",
                 min = 0,
                 max = 50,
                 step=1,
                 value = 0),
    selectInput("ASAP", 
                label = "ASAP",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("PIN", 
                label = "PIN",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("Statin_administration", 
                 label = "Statin medication",
                 choices= c("No","Yes"),
                selected = "No"),
    numericInput("Statin", 
                label = "Statin medication (months)",
                min = 0,
                max = 50,
                step=1,
                value = 0),
    selectInput("Hypercolesterol", 
                label = "Hypercholesterol",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("Obesity", 
                label = "Obesity",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("Low_HDL", 
                label = "Low HDL cholesterol",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("Diabetes", 
                label = "Diabetes",
                choices = c("No","Yes"),
                selected = "No"),
    numericInput("Andidiabtics_months", 
                 label = "Antidiabetics medication (months)",
                 min = 0,
                 max = 50,
                 step=1,
                 value = 0),
    selectInput("Hypertension", 
                label = "Hypertension",
                choices = c("No","Yes"),
                selected = "No"),
    numericInput("Hypertension_months", 
                 label = "Antihypertensives medication (months)",
                 min = 0,
                 max = 50,
                 step=1,
                 value = 0),
    selectInput("Low_glomerular", 
                 label = "Low glomerular filtration rate",
                choices = c("No","Yes"),
                selected = "No"),
    selectInput("Low_albumine", 
                label = "Low albumine level",
                choices = c("No","Yes"),
                selected = "No"),
    sliderInput("Thr", "Threshold probability:",
                min=0,
                max=100,
                step=0.1,
                value=3.9
    )),
    p("The calculator will offer you the probabilities of Prostate cancer (PCa), and results will be proposed as favorable (green) or unfavorable (red) after applying the Threshold probability selected. Using the threshold point, the PCa wrongly classified and biopsied avoided below the cutoff point are displayed in clinical utility curve for all data."),
    plotOutput("distPlottt")
 )


server<-shinyServer(function(input, output) {
  githubURL1 <- "https://github.com/lmesteban/USS/raw/master/AI4HealthyAging_RC.Rdata"
  load(url(githubURL1))
  githubURL2 <- "https://github.com/lmesteban/USS/raw/master/PCaROC.Rdata"
  load(url(githubURL2))
 
  
  output$distPlottt <- renderPlot({
    thr<-seq(0,1,by=0.001)
    sens<-coords(roc_PCa,seq(0,1,by=0.001),ret="sens")[,1]
    spec<-coords(roc_PCa,seq(0,1,by=0.001),ret="spec")[,1]
    
    
    Cutoff<-thr*100
    UB<-(1-sens)*100
    SB<-((1-sens)*509+(spec)*16763)/(16763+509)*100
    
    Predicted<-data.frame(input$PSA,input$Age,input$PSA_4_10,input$PSA_10,if(input$ASAP=="No") 0 else 1,
                      if(input$PIN=="No") 0 else 1,if(input$Statin_administration=="No") 0 else 1,input$Statin,
                      if(input$Hypercolesterol=="No") 0 else 1,if(input$Obesity=="No") 0 else 1,if(input$Low_HDL=="No") 0 else 1,
                      if(input$Diabetes=="No") 0 else 1,input$Andidiabtics_months,
                      if(input$Hypertension=="No") 0 else 1 ,input$Hypertension_months, 
                      if(input$Low_glomerular=="No") 0 else 1,if(input$Low_albumine=="No") 0 else 1)
    names(Predicted)<-c("ultimo_psa","edad",#"edad_factor",
                        "n_psa_entre_4_y_10","n_psa_mayor_que_10","ASAP","PIN","estatinas",
                        "duracion_estatinas","hipercolesterolemia","obesidad","colesterol_HDL_bajo",
                        "diabetes","duracion_antidiabeticos","hipertension_o_antihipertensivos",
                        "duracion_antihipertensivos",
                        "filtrado_glomerular_bajo_reciente_one_year",
                        "albumina_baja_reciente_one_year")
    PCaP<-predict(AI4HealthyAging_RC,newdata=as.matrix(Predicted))*100
    
      
    #layout(matrix(c(1,1,2,2,2,2,2,2),4,2,byrow=T))
    layout(matrix(c(1,1,2,2,2,2,2,2),4,2,byrow=T))
    par(cex.axis=1.3) 
    par(cex.lab=1.3)
    barplot(PCaP,col=c(if(PCaP>input$Thr) 'red' else 'darkgreen')
            #heat.colors(18)
            ,cex.main=1.8,horiz=TRUE,border="dark blue",xlim=c(0,110),ylim=c(0,0.1),width=0.07,beside=TRUE,space=0.1,xaxp=c(0,100,50),offset=0,
            main=c(paste("Probability of PCa=",round(PCaP,digits=2),"%"))
            #legend=c(paste("Probability of HGPCa=",round(PCaP,digits=2),"%")),args.legend = list(x = "top",cex=1.4,bty="n")
    )
    
    par(cex.lab=1.2)
    par(cex.main=1.2)
    par(cex=1.1)
    par(cex.axis=1)
    plot(Cutoff,UB,type="l",xlab="Threshold probability point",ylab="Percentage",main='Clinical utility curve',col="blue",ylim=c(-5,100))
     axis(1,seq(0,100,by=5),tck=1,col = "lightgray",lty="dotted")
     axis(2,seq(0,100,by=10),tck=1,col = "lightgray",lty="dotted",cex=1.1)
     leg.txt <- c("Undetected PCa","Saved MRI or biopsies")
    legend("bottomright", leg.txt,text.width=22, bty="n", col = c("blue","red"),     text.col = "black", lty = c(1,1), pch = c(-3, -1),     merge = TRUE)
    lines(Cutoff,SB,type="l",xlab="Threshold point",ylab="Percentage",main='Undetected HGPCa + Saved biopsy',col="red")
    lines(Cutoff,UB,type="l",xlab="Threshold probability point",ylab="Percentage",main='Clinical utility curve',col="blue",ylim=c(-5,100))
    # 
     points(input$Thr,round(UB[which(Cutoff==paste(input$Thr))],digits=1),col="blue",pch=7)
     points(input$Thr,round(SB[which(Cutoff==paste(input$Thr))],digits=1),col="red",pch=7) 
    lines(c(input$Thr,input$Thr),c(-10,round(SB[which(Cutoff==paste(input$Thr))],digits=1)),lty=3)
    lines(c(-10,input$Thr),c(round(UB[which(Cutoff==paste(input$Thr))],digits=1),round(UB[which(Cutoff==paste(input$Thr))],digits=1)),lty=3)
    lines(c(-10,input$Thr),c(round(SB[which(Cutoff==paste(input$Thr))],digits=1),round(SB[which(Cutoff==paste(input$Thr))],digits=1)),lty=3)
    text(input$Thr+4,-5,paste(round(input$Thr,digits=1),"%"))
    text(1,round(SB[which(Cutoff==paste(input$Thr))],digits=1)+4,paste(round(round(SB[which(Cutoff==paste(input$Thr))],digits=1),digits=1),"%"),col="red")
    text(1,round(UB[which(Cutoff==paste(input$Thr))],digits=1)+4,paste(round(round(UB[which(Cutoff==paste(input$Thr))],digits=1),digits=1),"%"),col="blue")
     
  })
 })



shinyApp(ui = ui, server = server)