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
#source("./functies/functies_box.R")


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
            h2("Intro"),
            br(" "),
            hr(style = "border-top: 1px solid #000000;"),
            p("... ,"),
            br(" "),
            p("bla bla bla bla - intro tekst "),
            hr(style = "border-top: 1px solid #000000;")
            ),
    # rijk ministeries -------------------
    tabItem(tabName = "lijst",
            h2("De lijst door de jaren heen"),
            fluidRow(
              #valueBoxOutput(
              #  outputId = "valuebox_zv_percentage",
              #  width = 3),
              #valueBoxOutput(
              #  outputId = "valuebox_aantal_zieken",
              #  width = 4),
              #valueBoxOutput(
              #  outputId = "valuebox_aantal_ziekmeldingen",
              #  width = 4),
            ),
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
                          selected = "AC/DC"),
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
              )
            ),
    ),
    # voorspellen ------------------------
    tabItem(tabName = "voorspellen",
            h2("Voorspellen"),
            box(
              title = "Inputs",
              background = "black",
              width = 3,
              radioButtons(inputId = "onderdelen_4",
                           label   = "artiest:", 
                           c("item 1" = "item1",
                             "item 2" =  "item2", 
                             "item 3" = "item3"),
                           selected = "item1")
            ),
            box(
              title = "Voorspelling obv xxx model", status = "primary",
              solidHeader = TRUE,
              width = 8,
              br(" "),
              #plotOutput("g_voorspelling"),
              br(" "),
              p("Voor de liefhebbers en transparantie zijn de specificaties 
                 van het gebruikte model hieronder toegevoegd:")#,
              #verbatimTextOutput("tekst_a")
            ))
  )
)

ui <- dashboardPage(header, sidebar, body)


# server ---------------------------------------------------------------
server <- function(input, output, session) {
  
  # valuebox_zv_percentage ---------------------------
  #valuebox_zv_perc <- reactive(vb_zv_perc(afk = input$onderdelen_2))
  #valuebox_zv_kleur <- reactive(vb_zv_kleur(afk = input$onderdelen_2))
  #valuebox_zv_icoon <- reactive(vb_zv_icoon(afk = input$onderdelen_2))
  #output$valuebox_zv_percentage <- renderValueBox({
  #  valueBox(
  #    value = paste0(valuebox_zv_perc(),"%"),
  #    subtitle = "huidig zv%",
  #    color = valuebox_zv_kleur(),
  #    icon = icon(valuebox_zv_icoon())
  #  )
  #})

  # valuebox_aantal_zieken ---------------------------
  #valuebox_aantal_z <- reactive(vb_aantal_zieken(afk = input$onderdelen_2))
  #output$valuebox_aantal_zieken <- renderValueBox({
  #  valueBox(
  #    value = valuebox_aantal_z(),
  #    subtitle = "huidig aantal zieken",
  #    color = "yellow",
  #    icon = icon("user")
  #  )
  #})

  # valuebox_aantal_ziekmeldingen ---------------------------
  #valuebox_aantal_zm <- reactive(vb_aantal_ziekmeldingen(afk = input$onderdelen_2))
  #output$valuebox_aantal_ziekmeldingen <- renderValueBox({
  #  valueBox(
  #    value = valuebox_aantal_zm(),
  #    subtitle = "aantal ziekmeldingen laatste week",
  #    color = "blue",
  #    icon = icon("rectangle-list")
  #  )
  #})
  
  # ---------------------------------------
  #observeEvent(input$artiest_keuze, {
  #  updateSelectInput(session=session,
  #                    inputId = "artiest_keuze",
  #                    choices = f_artiesten_selecteren(
  #                      jaar1 = input$vanaf_jaar)
  #                    )
  #  })
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

