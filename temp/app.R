warpbreaks_2 <- warpbreaks
warpbreaks_2$tension <- as.character(warpbreaks_2$tension)
warpbreaks_2$tension[warpbreaks_2$wool == "B" & warpbreaks_2$breaks > 25] <- "W"
warpbreaks_2$tension <- factor(warpbreaks_2$tension)

f_sel <- function(aaa = input$var1) {
  warpbreaks_2 %>%
    filter(wool == aaa) %>%
    select(tension)
}


library(tidyverse)
library(shiny)
shinyApp(
  ui = fluidPage(
    selectInput(inputId = "var1", 
                label = "Variable wool:", 
                choices = warpbreaks_2$wool),
    selectInput(inputId =  "var2", 
                label =  "Variables tension:",
                choices = warpbreaks_2$tension),
    plotOutput("plot1")
  ),
  server = function(input, output,session) {
    
    observeEvent(input$var1,{
      updateSelectInput(session=session,
                        inputId = "var2",
                        choices = f_sel(input$var1)
                        )
    })
    
    aaa <- reactive(
      ggplot(data = (warpbreaks_2 %>% 
               filter(wool == input$var1,
                      tension == input$var2))) +
        geom_bar(aes(breaks)))
    
    output$plot1 <- renderPlot({aaa()})
    
  }
)