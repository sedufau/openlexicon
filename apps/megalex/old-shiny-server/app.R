#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# call main library
library(shiny)
library(shinydashboard) # here, we will use the dashboard logic and ui
#library(shinyjs) # shinyjs is a CRAN library made to use javascript with R syntax
library(stringi) # for string manipulation


# Define UI for application
ui <- dashboardPage(
  
  ## Header (disabled is nicer actually)
  dashboardHeader(title = "ANR MEGALEX", disable="true"),
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("MEGALEX project", tabName = "home", icon = icon("home")),
      img(src="anr_small.png", style="float: left; position: relative; left: 10%;")
    )
  ),
  dashboardBody(
    
    # tabItems for each sidebarMenu entry
    tabItems(
      
      
      ### HOME UI
      tabItem(tabName = "home",
              tags$h3("Megastudies of visual and auditory word recognition in French"),
              tags$h3(tags$a(href="http://lexique.org", "MEGALEX is now transferred to http://lexique.org")),
              tags$h4(tags$a(href="http://lexique.org", "http://lexique.org - The link to Open Lexique")),
              tags$h4(tags$a(href="http://lexique.org:3838/megalex", "http://lexique.org:3838/megalex - The link to a beta-version of a shiny app")),
              tags$p("For more than a century, researchers in psycholinguistics, cognitive psychology, and cognitive science have tried to understand the mental processes underlying visual and auditory word recognition (see e.g., Adelman, 2011; Balota, Yap & Cortese, 2006; Ferrand, New, Brysbaert, Keuleers, Bonin, Méot, Augustinova & Pallier, 2010; Grainger & Holcomb, 2009; Grainger & Ziegler, 2011; Dahan & Magnuson, 2006; Pisoni & Levi, 2007). To date, nearly all research has been based on small studies involving a limited set of monosyllabic words selected according to factorial designs, with a limited number of independent variables matched on a series of control variables. The present project aims to supplement previous studies with a new approach, the 'megastudy approach' (see, e.g., Balota et al., 2007, 2013), by (1) using multiple regression designs involving very large-scale stimuli sets; (2) investigating the cognitive processes underlying the visual and auditory word recognition of more complex words, i.e. polysyllabic and polymorphemic words; and (3) using the psychophysical approach (with the repeated measures design) developed recently by Keuleers, Lacey, Rastle, and Brysbaert (2012)."),
              tags$p("This project had two main phases. Phase 1 of the project has collected reaction times and percent errors in the visual lexical decision task on about 28,000 French words and 28,000 pseudowords with a small group of participants (n=100). The 28,000 words (mainly polysyllabic and polymorphemic words of different lengths and frequencies) were selected among the 130,000 distinct lexical entries available in Lexique (www.lexique.org; New, Pallier, Brysbaert, & Ferrand, 2004). We have also included inflected forms (such as feminine, plural, and verbal forms)."),
              tags$p("Phase 2 of the project has collected reaction times and percent errors on 17,876 words and the same number of pseudowords, in a modality never tested before at such large-scale, namely the auditory modality. Megastudies are virtually nonexistent in auditory word recognition research and the literature on auditory word recognition has been dominated by experimental studies. It was therefore crucial to provide and explore an auditory analogue of what has been already done in visual word recognition. Presenting auditory stimuli has implied more effort than presenting visual stimuli, but it was worth trying because factors specific to the auditory modality are influencing auditory word recognition (e.g., phonological neighborhood density, stimulus duration, uniqueness point, etc.) in plus of the usual factors found in visual word recognition (e.g., word frequency, length in letters and syllables, semantic neighbors, etc.)."),
              tags$p("To succeed in the realization of this ambitious project, we have put together a dynamic and interdisciplinary team (whose members have already worked and published together), which is extremely competent in the areas of psycholinguistics and data mining."),
              tags$p("The collected reaction times and the sophisticated analyses we will conduct will allow us to (1) understand more precisely the functional architecture of the different levels of processing involved in both visual and auditoy word recognition, (2) detail the nature of the representation on which these processes apply, and (3) study the type of coding (orthographic, phonological, morphological, semantic) used by these different levels of processing. These results will be crucial for models of reading and auditory word recognition. Overall, this work will lead us to a better understanding of factors at play in visual and auditory word recognition."),tags$h3("Partners"),
              tags$p("LAPSCO Laboratoire de Psychologie Sociale et Cognitive (CNRS UMR 6024)",
                     tags$br(),
                     "LPC Laboratoire de Psychologie Cognitive (CNRS UMR7290)",
                     tags$br(),
                     "LPNC Laboratoire de Psychologie et NeuroCognition (CNRS UMR 5105)",
                     tags$br(),
                     "UNICOG Neuroimagerie Cognitive (INSERM U992)",
                     tags$br(),tags$br(),
                     "Supported by the French Agence Nationale de la Recherche (ANR) -  ANR-12-CORP-0001"),
              tags$p( tags$strong("About"),
                      tags$br(),
                      "Program coordinator: Ludovic Ferrand",
                      tags$br(),
                      "Investigators: L. Ferrand, A. Méot, J. Grainger, S. Dufau, S. Mathot, E. Spinelli, C. Pallier, B. New, P. Bonin",
                      tags$br(),
                      "Web application: Stephane Dufau "),
              tags$p( tags$strong("Contact"),
                      tags$br(),
                      "Ludovic Ferrand")
              
      )
      
      
    
    
    )
  
  
  )

)



  
# Define server logic 
server <- function(input, output, session) {
  
 
  
  } # end of server

# Run the application 
shinyApp(ui = ui, server = server)




