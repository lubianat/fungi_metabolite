#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(readxl)
library(dplyr)
library(tidyr)
fungos <-
  read_xlsx('./fungos.xlsx')

fungos2 <-
  fungos %>% select(c('Metabolite', `UV absorption (nm) in % of UV-max`))

fungos3 <-
  fungos2 %>% filter(!`UV absorption (nm) in % of UV-max` %in% c(NA, 'ND', 'End'))

fungos4 <-
  fungos3 %>% separate_rows(`UV absorption (nm) in % of UV-max`, sep = ',')

fungos5 <-
  fungos4 %>% separate(
    col = `UV absorption (nm) in % of UV-max`,
    sep = '\\(',
    into = c('wavelength', 'percentage_of_max')
  )
fungos6 <- fungos5

fungos6$percentage_of_max <-
  gsub(')', '', fungos6$percentage_of_max)

fungos7 <- fungos6[!is.na(as.numeric(fungos6$wavelength)), ]

fungos7$percentage_of_max[is.na(fungos7$percentage_of_max)] <- 100

fungos8 <- fungos7[!is.na(as.numeric(fungos7$wavelength)), ]




# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Fungi Metabolite Searcher"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "maxw",
        "Max wavelength",
        min = 200,
        max = 600,
        value = 350
      )
      ,
      sliderInput(
        "minw",
        "Min wavelength",
        min = 200,
        max = 600,
        value = 250
      ),
      sliderInput(
        "minp",
        "Min percentage",
        min = 0,
        max = 100,
        value = 0
      ),
      sliderInput(
        "maxp",
        "Max percentage",
        min = 0,
        max = 100,
        value = 100
      )
      
    ),
    # Show a plot of the generated distribution
    mainPanel(textOutput('bla'),
              tableOutput("STAR"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  metabolite <- eventReactive({
    input$maxw
    input$minw
    input$maxp
    input$minp
  }, {
    fungos9 <-
      fungos8 %>% filter(
        wavelength <=  input$maxw,
        wavelength >=  input$minw,
        percentage_of_max <= input$maxp,
        percentage_of_max >= input$minp
      )
    return(fungos9)
  })
  output$bla <- renderText(paste0('Possible metabolites:'))
  output$STAR <- renderTable({
    metabolite()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
