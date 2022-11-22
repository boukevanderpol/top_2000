# 
# 
# 
# 
# 
# 
# 
# 

library(shiny)
library(tidyverse)
library(lubridate)

# test

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
        sidebarPanel("keuze panel",
            selectInput(inputId = "artiest",
                        label = "Kies een artiest:",
                        choices = artiesten,
                        selected = "Ac/Dc",
                        multiple = F
                        ),
            checkboxInput(inputId = "checkbox_1", 
                          label = "Song tonen", 
                          value = F #
                           )
                     ),
        
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
            dplyr::filter(artiest %in% input$artiest) 
#            dplyr::filter(artiest %in% "Ac/Dc") 
        if (input$checkbox_1 == TRUE) {
             
            aaa <- dplyr::distinct(lijst_2, song)
            bbb <- as.character(aaa$song)
            lijst_3 <- tibble()
            for (i in base::seq_along(bbb)) {
                ccc <- lijst_2 %>%
                    filter(song == bbb[[i]]) %>%
                    #select(song, jaar_lijst) %>%
                    unique()
                ddd <- ccc %>%
                    filter(jaar_lijst == base::min(jaar_lijst))
                lijst_3 <- dplyr::bind_rows(lijst_3, ddd)
            }
        } else {
            lijst_3 <- lijst %>% 
                dplyr::filter(artiest %in% input$artiest) %>%
                dplyr::mutate(song = NA)
        }
        
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
            geom_label(data = lijst_3,
                       aes(x = jaar_lijst, 
                           y = ranglijst,
                           label=lijst_3$song,
                           na.rm = T,
                           fontface = "bold"), 
                       check_overlap = TRUE,
                       size=4, hjust=0) +
            guides(colour = "none") +
            ylim(1, 100) +
            xlim(2012, 2021) + 
            scale_y_reverse() + 
            theme_minimal()
        
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
