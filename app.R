# ========================================================================
# shiny app over de top 2000
# 
# 
# 
# 
# datum: 23 november 2022
# Bouke van der Pol, boukevanderpol@gmail.com   
# ========================================================================

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinydashboard))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tsibble))
#suppressPackageStartupMessages(library(tsibbledata))
#suppressPackageStartupMessages(library(fabletools))
#suppressPackageStartupMessages(library(fable))
#suppressPackageStartupMessages(library(feasts))
suppressPackageStartupMessages(library(urca))
suppressPackageStartupMessages(library(ggrepel))

laatste_jaar_top_2000 <- 2021

source("./functies/functies_laden.R")
source("./functies/functies_grafieken.R")
source("./functies/functies.R")
source("./functies/functies_box.R")


header <- dashboardHeader(title = "Top 2000")

sidebar <-dashboardSidebar(
  sidebarMenu(
    menuItem("Intro", tabName = "intro", icon = icon("th", verify_fa = FALSE)),
    menuItem("De lijst", tabName = "lijst", icon = icon("th", verify_fa = FALSE)),
    menuItem("Voorspellen", tabName = "voorspellen", icon = icon("th", verify_fa = FALSE))
  )
)

body <- dashboardBody(
  tabItems(
    # intro ---------------------------
    tabItem(tabName = "intro",
            #h2("Intro"),
            #br(" "),
            hr(style = "border-top: 1px solid #000000;"),
            p("Beste mensen,"),
            br(" "),
            p("Op het volgende tabblad kan je zien op welke wijze een artiest 
              met zijn songs door de jaren in de top 2000 heeft gestaan."),
            #br(" "),
            p("Het is mogelijk dat nog ergens een foutje in de data zit of dat een 
              functionaliteit een bug heeft. Als je zoiets ziet, stuur dan 
              een mail aan mij --> boukevanderpol ad gmail.com. Dan kijk ik 
              ernaar en los ik het op voor volgend jaar. "),
            hr(style = "border-top: 1px solid #000000;")
            ),
    # rijk ministeries -------------------
    tabItem(tabName = "lijst",
            h2("De lijst door de jaren heen"),
            box(
              title = "Inputs", #status = "warning",
              background = "black",
              width = 3,
              #sliderInput(inputId = "vanaf_jaar",
              #            label = "Vanaf welke jaar:",
              #            min = 1999,
              #            max = laatste_jaar_top_2000 - 1,
              #            value = 2012),
              selectizeInput(inputId = "artiest_keuze", label = "artiest",
                          choices = f_artiesten_selecteren(),
                          selected = "Chris Isaak"),
              selectizeInput(inputId = "artiest_song_keuze", label = "song",
                          choices = f_artiestsong_selecteren(),
                          selected = "(leeg)")
            ),
            fluidRow(
              box(
                title = "overzicht lijst", status = "primary",
                solidHeader = TRUE,
                width = 8,
                br(" "),
                plotOutput("graf_lijnen")
              ),
              valueBoxOutput(
                outputId = "valuebox_hoogste_positie",
                width = 11)
              
            ),
    ),
    # voorspellen ------------------------
    tabItem(tabName = "voorspellen",
            #h2("Voorspellen"),
            #box(
            #  title = "Inputs",
            #  background = "black",
            #  width = 3,
            #  radioButtons(inputId = "onderdelen_4",
            #               label   = "artiest:", 
            #               c("item 1" = "item1",
            #                 "item 2" =  "item2", 
            #                 "item 3" = "item3"),
            #               selected = "item1")
            #),
            box(
              title = "Voorspelling ...", status = "primary",
              solidHeader = TRUE,
              width = 8,
              br(" "),
              #plotOutput("g_voorspelling"),
              br(" "),
              p("In aanbouw.....")#,
              #verbatimTextOutput("tekst_a")
            ))
  )
)

ui <- dashboardPage(header, sidebar, body)


# server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # valuebox_zv_percentage ---------------------------
  valuebox_hoogste_positie <- reactive(
    vb_hoogste_positie(artiest1 = input$artiest_keuze,
                       song1 = input$artiest_song_keuze))
  
  output$valuebox_hoogste_positie <- renderValueBox({
    valueBox(
      value = valuebox_hoogste_positie(),
      subtitle = "hoogste positie - song - jaar",
      color = "green",          #valuebox_zv_kleur(),
      icon = icon("arrow-up")   #valuebox_zv_icoon())
    )
  })

  observeEvent(input$artiest_keuze, {
    updateSelectizeInput(session=session,
                         inputId = "artiest_song_keuze",
                         choices = f_artiestsong_selecteren(
                           #jaar1 = input$vanaf_jaar,
                           artiest1 = input$artiest_keuze)
                         )
  })
  # grafiek_lijnen -----------------------------------
  grafiek_lijnen <- reactive(g_lijnen(
    x_waarde_min = 1999, #input$vanaf_jaar,
    artiest1     = input$artiest_keuze,
    song1        = input$artiest_song_keuze
    ))
  output$graf_lijnen <- renderPlot({grafiek_lijnen()}, 
                                   res = 96)
  
  

  
  # tekst_a ------------------------------------------
  #grafiek_voorspelling_b <- reactive(g_voorspellen_b(
  #  aantal_maanden = input$aantal_mnd_4,
  #  afk = input$onderdelen_4))
  #output$tekst_a <- renderPrint({grafiek_voorspelling_b()})
  
}

# Run the application 
shinyApp(ui = ui, server = server)

