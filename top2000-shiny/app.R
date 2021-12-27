#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(lubridate)



# gegevens inlezen en ordenen
lijst <- readr::read_csv2(file = "lijst.csv", 
                          col_names = T,
                          col_types = c("ncccn"))
lijst <- lijst %>%
    mutate(artiest = stringr::str_to_title(lijst$artiest))
artiesten  <- sort(unique(lijst$artiest))
temp <- lijst %>%
    filter(artiest == "Ac/Dc")
songs <- sort(unique(temp$song))


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    # Application title
    titlePanel("Top 2000"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel("sidebar panel",
            selectInput(inputId = "artiest",
                        label = "Kies een artiest:",
                        choices = artiesten,
                        selected = "Ac/Dc",
                        multiple = F
                        )
        ),
        
        #sidebarPanel("sidebar panel",
        #             checkboxInput("do2", "Song tonen", value = F)
        #),
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lijst_top2000"),
           tableOutput("tabel_songs")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$lijst_top2000 <- renderPlot({

                
        # gegevens inlezen en ordenen
        lijst_2 <- lijst %>% 
            dplyr::filter(artiest %in% input$artiest) # "Ac/Dc") #
        lijst_3 <- lijst_2 %>% 
            dplyr::filter()
        #songs <- lijst %>%
        #    dplyr::filter(artiest %in% input$artiest) %>% #,
        #                  song %in% input$lied) %>%
        #    sort(unique(lijst$song))

        # grafiek
        ggplot() + 
            geom_line(data = lijst, 
                      aes(x = jaar_lijst, 
                          y = ranglijst,
                          colour = song)) +
            geom_line(data = lijst_2,
                      aes(x = jaar_lijst, 
                          y = ranglijst,
                          colour = song),
                      size = 2) + 
            geom_text(data = lijst_3,
                      aes(x = jaar_lijst, 
                          y = ranglijst,
                          label=lijst_2$song), 
                      size=4, hjust=0) +
            guides(colour = "none") + 
            ylim(0, 100)
        
    })
    
    output$tabel_songs <- renderTable({
        lijst %>%
            dplyr::filter(artiest %in% input$artiest) %>% # "Ac/Dc") %>% #
            select(song) %>% 
            unique() %>%
            arrange()
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
