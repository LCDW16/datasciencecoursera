library(shiny)
shinyUI(fluidPage(
  titlePanel("Predict miles per gallon (mpg) from number of cylinder and weight of the car"),
  sidebarLayout(
    sidebarPanel(
      h4("Check only one of the checkboxes!"),
      numericInput("Inputcyl", "How many cylinders has the car?", value = 4, min = 4, max = 8, step = 2, width = NULL),
      checkboxInput("showModel1", "Show model: mpg from number of cylinder", value = TRUE),
      sliderInput("Sliderwt", "How heavy is the the car?", value = 2, min = 1, max = 6),
      checkboxInput("showModel2", "Show Model: mpg from weight of the car", value = TRUE),
      checkboxInput("showModel3", "Show Model: mpg from cylinder and weight", value = TRUE),
      submitButton("Submit")
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Output", br(),
                  plotOutput("plot1"),
                  h3("Predicted mpg from number of cylinder:"),
                  textOutput("pred1"),
                  h3("Predicted mpg from weight of the car:"),
                  textOutput("pred2"),
                  h3("Predicted mpg from cylinder and weight of the car:"),
                  textOutput("pred3")),
                  tabPanel("Documentation",
                           p(h4("Predict mpg from cylinder and/or weight Application")),
                           br(),
                           helpText(" - Select number of cylinders and/or the weight of the car."),
                           helpText(" - Select one of the models by checking one of the checkboxes."),
                           helpText(" - Then press Submit."),
                           helpText(" - This application will then calculate the predicted value of mpg based on selected input and model."),
                           helpText(" - This application will also show the plot including the predicted value based on the selected input and model."))
    )
  )
)))
