library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("Alternative to Sievert Model"),
  sidebarLayout(      
    sidebarPanel(
      radioButtons("race", label = h3("African American (AA)", style="font-size:170%; font-family:Arial;font-weight: bold"),
                   choices = list("Yes" = 1, "No/ Any Other Race" = 0), 
                   selected = 1),
      div(
        numericInput("age", "Age:", 10), style="font-size:170%; font-family:Arial;",
      numericInput("ga", "Gestational Age (weeks):", 10), style="font-size:170%; font-family:Arial;",
    numericInput("parity", "Parity:", 1), style="font-size:170%; font-family:Arial;",
  numericInput("bmi", "Body Mass Index at admission (BMI):", 21), style="font-size:170%; font-family:Arial;",
  numericInput("bs", "Simplified Bishop Score:", 1), style="font-size:170%; font-family:Arial;",
numericInput("pprom", "Preterm Premature ruprure of membranes:", 1), style="font-size:170%; font-family:Arial;",
numericInput("abruption", "Abruption:", 10), style="font-size:170%; font-family:Arial;"),

      submitButton(text = "Calculate"),
      hr(),
      helpText("Please make your selection and click calculate to calculate the 'p' value of the final model")),
    mainPanel(
      htmlOutput("text_calc"))))
)

server <- shinyServer(function(input, output,session){
  output$text_calc <- renderText({
    race <- as.numeric(input$race)
    age <- as.numeric(input$age)
    ga <- as.numeric(input$ga)
    parity <- as.numeric(input$parity)
    bmi <- as.numeric(input$bmi)
    bs <- as.numeric(input$bs)
    pprom <- as.numeric(input$pprom)
    abruption <- as.numeric(input$abruption)
    e<-2.718
    # validate(
    #   need(input$hematocrit != "", "Please enter a valid continious pre-operative hematocrit value")
    # )
    # hematocrit<-as.numeric(input$hematocrit)
    g<-0.903-0.056*age+0.074*ga-0.220*race+ 0.208*ifelse(parity>= 1 & parity <= 2, as.numeric(1),as.numeric(0))
    +0.808*ifelse(parity >=3, as.numeric(1),as.numeric(0))-0.028*bmi-0.335*ifelse(bs>= 1 & bs <= 2, as.numeric(1),as.numeric(0))
    -0.043*ifelse(bs>= 3 & bs <= 4, as.numeric(1),as.numeric(0))+0.524*pprom-0.992*abruption
    paste0(
      "The 'p' value for a ",
          ifelse(race=='1',"Black individual ","Non-Black individual "),
          "aged ", parity, " years with a gestatioal age of ",ga," weeks, parity of ",parity,
          ", bmi of ",bmi,", simplified bishop score of ",bs,", PPROM of ",pprom," and abruption of ",abruption," is ",  "<font size= \"4px\"><b>",
          "<font color=\"#FF0000\"><b>",
          1/
            1+e^(-g), "</b></font>"
      )
  })
})

shinyApp(ui = ui, server = server)
