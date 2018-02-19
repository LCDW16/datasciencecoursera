library(shiny)
shinyServer(function(input, output) {
  attach(mtcars)
  model1 <- lm(mpg ~ cyl, data = mtcars)
  model2 <- lm(mpg ~ wt, data = mtcars)
  model3 <- lm(mpg ~ cyl + wt, data = mtcars)

  model1pred <- reactive({
    cylInput <- input$Inputcyl
    predict(model1, newdata = data.frame(cyl = cylInput))
  })

  model2pred <- reactive({
    wtInput <- input$Sliderwt
    predict(model2, newdata = data.frame(wt = wtInput))
  })

  model3pred <- reactive({
    cylInput <- input$Inputcyl
    wtInput <- input$Sliderwt
    predict(model3, newdata = data.frame(cyl = cylInput, wt=wtInput))
  })

  output$plot1 <- renderPlot({

    if(input$showModel1){
      cylInput <- input$Inputcyl
      plot(mtcars$cyl, mtcars$mpg, xlab = "number of cylinders",
         ylab = "miles per gallon", bty = "n", pch = 16,
         xlim = c(4, 8), ylim = c(10, 35))
      abline(model1, col = "red", lwd = 2)
      points(cylInput, model1pred(), col = "red", pch = 16, cex = 2)
      output$pred1 <- renderText({
        model1pred()
      })
      output$pred2 <- renderText({
          NULL
      })
      output$pred3 <- renderText({
        NULL
      })

    }
    else if(input$showModel2 ){
      wtInput <- input$Sliderwt
      plot(mtcars$wt, mtcars$mpg, xlab = "weight of the car",
           ylab = "miles per gallon", bty = "n", pch = 16,
           xlim = c(1, 6), ylim = c(10, 35))
      abline(model2, col = "blue", lwd = 2)
      points(wtInput, model2pred(), col = "blue", pch = 16, cex = 2)
      output$pred2 <- renderText({
         model2pred()
     })
      output$pred1 <- renderText({
        NULL
      })
      output$pred3 <- renderText({
        NULL
      })

    }
    else if(input$showModel3) {
    library(ggplot2)
      cylInput <- input$Inputcyl
      wtInput <- input$Sliderwt
     g <- ggplot(mtcars, aes(wt,mpg))+ geom_point(size = 2)+ facet_grid(.~cyl)+ labs(x="weight",y="miles per gallon")
    pred_point <- data.frame(wt = wtInput, mgp = model3pred(), cyl = cylInput)
    p <- g + geom_point(data = pred_point, aes(x=wtInput, y=model3pred(), cyl=cylInput), colour="green", size = 4)
    print(p)
    output$pred3 <- renderText({
      model3pred()
    })
    output$pred1 <- renderText({
      NULL
    })
    output$pred2 <- renderText({
      NULL
    })
    }
  })

})
