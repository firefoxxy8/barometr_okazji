library(shiny)
library(shinythemes)

# Define UI for dataset viewer application
fluidPage(
  theme = shinytheme("united"),
  
  # Application title.
  titlePanel('Barometr okazji - mieszkania na sprzedaż - Warszawa'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('Aplikacja służy ocenie atrakcyjności ogłoszenia o sprzedaży mieszkania.',
               'Działa dla ogłoszeń z Warszawy zamieszczonych w otodom.pl. ',
               'Aplikacja znajduje podobne ogłoszenia i na ich podstawie',
               'decyduje o tym, czy proponowana cena jest atrakcyjna.'),
      
      textInput('url', label = h4('Link do ogłoszenia'), value = 'https://www.otodom.pl/oferta/mieszkanie-191-m-warszawa-ID3fGRY.html'),
      
      numericInput('obs', label = h4('Liczba porównywanych ogłoszeń:'), 20),
      
      submitButton('Sprawdź ogłoszenie')
    ),
    
    mainPanel(
      h4('Dane ogłoszenia'),
      h4('Rozkład cen podobnych mieszkań'),
      hr(),
      h4('Podobne mieszkania'),
      dataTableOutput('sims')
      
    )
  )
)