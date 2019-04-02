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
      menuItem("By word lists", tabName = "bylists", icon = icon("list-alt")),
      menuItem("By language variables", tabName = "byvariables", icon = icon("bar-chart")),
      menuItem("Raw files", tabName = "rawfiles", icon = icon("file-o")),
      img(src="anr_small.png", style="float: left; position: relative; left: 10%;")
    )
  ),
  dashboardBody(
    
    # tabItems for each sidebarMenu entry
    tabItems(
      
      
      ### HOME UI
      tabItem(tabName = "home",
              tags$h3("Megastudies of visual and auditory word recognition"),
              tags$h3(style="color:red","Beta-version of the Shiny App - Please use Open Lexique"),
              tags$p("----------", tags$br(), "Lexique.org is the new home of the Megalex experiment (ask Open Lexique on the left side bar to download the published data).",
                     tags$br(),
                     "You can alternatively use this Shiny App in its beta version (unpublished data though).", tags$br(), "----------"),
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
              
      ),
      
      
      ### LIST UI
      tabItem(tabName = "bylists",
              
              #title
              h2("Analyse by word lists"),
              tags$h3(style="color:red", "DATA IN PREPARATION - NOT TO BE USED"),
              
              #create a grey area to identify the input section
              tags$div("style"="background-color:LightGray; padding:5px;",
                       
                       #textareas to be filled with stimuli
                       tags$h5("You can fill the following text areas with words, or click on LOAD EXAMPLE:"),
                       tags$table(id="bylisttable", 
                                  tags$tr( tags$td(tags$label("for"="stim_list1", "List 1")),
                                           tags$td(tags$label("for"="stim_list2", "List 2")),
                                           tags$td(tags$label("for"="stim_list3", "List 3")),
                                           tags$td(tags$label("for"="stim_list4", "List 4"))),
                                  tags$tr( tags$td(tags$textarea(id="stim_list1", rows=5, cols=30, "")),
                                           tags$td(tags$textarea(id="stim_list2", rows=5, cols=30, "")),
                                           tags$td(tags$textarea(id="stim_list3", rows=5, cols=30, "")),
                                           tags$td(tags$textarea(id="stim_list4", rows=5, cols=30, ""))
                                           )
                                  ),
                      
                       #output options (either plot & summary, or table, or even both)
                       checkboxGroupInput("list_output_type", "Output:",
                                          c("Plot & summary" = "Plot & summary", "Table" = "Table"),
                                          selected="Plot & summary", inline=TRUE),
                       
                       #button (necessary to stop the continuous refresh)
                       #tags$div("style"="float:left; padding:5px;", ""),#submitButton("Compute")),
                       tags$div("style"="float:left; padding:5px;", downloadButton('list_export_data', 'Export data')), # download data from lists
                       tags$div("style"="float:left; padding:5px;", actionButton('list_load_example', 'Load example lists')), # load example data for lists
                       tags$div("style"="padding:5px;", actionButton('list_clear_list', 'Clear lists')) # clear data for lists
                       
                       
              ),
              
              #below the input section: conditional panel based on chosen output type (a dynamic UI that reacts to user input)
              
              #plot & summary (single for all the text areas)
              conditionalPanel(
                condition = "input.list_output_type.indexOf('Plot & summary') != -1",
                
                #missing words in database
                tags$h4(style="color:red", "Missing words"),
                uiOutput("list_output_missing"),
                
                #plot
                tags$h4(style="color:red", "Plot & summary"),
                plotOutput("list_output_plots"),
                
                #basic statistics VISUAL
                tags$div(tags$strong("Visual list 1"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_visual_output_summary_list1")),
                tags$div(tags$strong("Visual list 2"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_visual_output_summary_list2")),
                tags$div(tags$strong("Visual list 3"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_visual_output_summary_list3")),
                tags$div(tags$strong("Visual list 4"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_visual_output_summary_list4")),
                #anova
                tags$div(tags$strong("ANOVA on visual data"), tags$i("(all lists)")),
                tags$div(verbatimTextOutput("list_visual_output_summary_anova")),
                
                #basic statistics AUDITORY
                tags$div(tags$strong("Audio list 1"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_audio_output_summary_list1")),
                tags$div(tags$strong("Audio list 2"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_audio_output_summary_list2")),
                tags$div(tags$strong("Audio list 3"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_audio_output_summary_list3")),
                tags$div(tags$strong("Audio list 4"), tags$i("(RTs in ms)")),
                tags$div(verbatimTextOutput("list_audio_output_summary_list4")),
                #anova
                tags$div(tags$strong("ANOVA on auditory data"), tags$i("(all lists)")),
                tags$div(verbatimTextOutput("list_audio_output_summary_anova"))
              ),
              
              #Tables (unique for each text areas if filled)
              conditionalPanel(
                condition = "input.list_output_type.indexOf('Table') != -1 && !(input.stim_list1 === '')",
                tags$h4(style="color:red", "Visual list 1"),
                dataTableOutput("list1_visual_output_table"),
                tags$h4(style="color:red", "Auditory list 1"),
                dataTableOutput("list1_audio_output_table")),
              conditionalPanel(
                condition = "input.list_output_type.indexOf('Table') != -1 && !(input.stim_list2 === '')",
                tags$h4(style="color:red", "Visual list 2"),
                dataTableOutput("list2_visual_output_table"),
                tags$h4(style="color:red", "Auditory list 2"),
                dataTableOutput("list2_audio_output_table")),
              conditionalPanel(
                condition = "input.list_output_type.indexOf('Table') != -1 && !(input.stim_list3 === '')",
                tags$h4(style="color:red", "Visual list 3"),
                dataTableOutput("list3_visual_output_table"),
                tags$h4(style="color:red", "Auditory list 3"),
                dataTableOutput("list3_audio_output_table")),
              conditionalPanel(
                condition = "input.list_output_type.indexOf('Table') != -1 && !(input.stim_list4 === '')",
                tags$h4(style="color:red", "Visual list 4"),
                dataTableOutput("list4_visual_output_table"),
                tags$h4(style="color:red", "Auditory list 4"),
                dataTableOutput("list4_audio_output_table"))
              
              #uiOutput("list_output")
              #tags$h4(style="color:red", "Plot"),
              #dataTableOutput("list_output_table"),
      ),
      
      
      ### VARIABLE UI
      tabItem(tabName = "byvariables",
              h2("Analyse by language variables"),
              tags$h3(style="color:red", "DATA IN PREPARATION - NOT TO BE USED"),
              tags$div(
                htmlOutput("variable_table", container = tags$div)
              ),
              tags$div("style"="float:left; padding:5px;", downloadButton('var_export_visual', 'Export VISUAL data')),
              tags$div("style"="padding:5px;", downloadButton('var_export_audio', 'Export AUDIO data')),
              tags$h4("Words selected:"),
              tags$div(verbatimTextOutput("TEST_THINGS_FOR_DEBUGGING"))
      ),
      
      
      ### RAW UI
      tabItem(tabName = "rawfiles",
              h2("Download raw files"),
              tags$h3(style="color:red", "DATA IN PREPARATION - NOT TO BE USED"),
              tags$div(tags$strong("Visual raw data (csv format)"),
                       tags$div("style"="padding:5px;", downloadButton('list_visual_export_all', 'Download VISUAL data'))), # download data from lists,
              tags$div(tags$strong("Auditory raw data (csv format)"),
                       tags$div("style"="padding:5px;", downloadButton('list_auditory_export_all', 'Download AUDITORY data')))
              )
      
      
    )
    
    
  )
  
  
)



#load the data for inter-session uses
load('../../rdata/Megalex-visual.RData')
load('../../rdata/Megalex-auditory.RData')

load("mAV.RData") # 2 data frames AUDIO and VISU
V = VISU
A = AUDIO

# string normalization (diacritic character)
string_normalize <- function(in1) {
  out1 <- stri_trans_nfc(unlist(strsplit(in1,"[[:space:][:punct:]]")))
  return(out1)
}
# get word index in databases
get_ind <- function(df,l) {
  out1 <- which(is.element(df$SPELLING, l))
  return(out1)
}
# get RTs
get_rt <- function(df,l) {
  out1 <- df[l, "RT"]
  return(out1)
}
# get missing words
get_missing <- function(df,l) {
  out1 <- which(!l %in% df$SPELLING)
  return(out1)
}

textInputSmall<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "numeric", value = value,...))
}

  
# Define server logic 
server <- function(input, output, session) {
  
  
  
  ### LIST ##################################################################
  #LIST - reactive functions (cache the results until associated widget values are changed)

  #get words from text areas
  list_word1 <- reactive({ string_normalize(input$stim_list1) }) 
  list_word2 <- reactive({ string_normalize(input$stim_list2) })
  list_word3 <- reactive({ string_normalize(input$stim_list3) })
  list_word4 <- reactive({ string_normalize(input$stim_list4) })

  #get the index of textarea words in the lexical database
  list_visual_ind1 <- reactive({ get_ind(V, list_word1()) })
  list_visual_ind2 <- reactive({ get_ind(V, list_word2()) })
  list_visual_ind3 <- reactive({ get_ind(V, list_word3()) })
  list_visual_ind4 <- reactive({ get_ind(V, list_word4()) })
  list_audio_ind1 <- reactive({ get_ind(A, list_word1()) })
  list_audio_ind2 <- reactive({ get_ind(A, list_word2()) })
  list_audio_ind3 <- reactive({ get_ind(A, list_word3()) })
  list_audio_ind4 <- reactive({ get_ind(A, list_word4()) })
  
  #get the database's rts - textarea words
  list_visual_rt1 <- reactive({ get_rt(V, list_visual_ind1()) })
  list_visual_rt2 <- reactive({ get_rt(V, list_visual_ind2()) })
  list_visual_rt3 <- reactive({ get_rt(V, list_visual_ind3()) })
  list_visual_rt4 <- reactive({ get_rt(V, list_visual_ind4()) })
  list_audio_rt1 <- reactive({ get_rt(A, list_audio_ind1()) })
  list_audio_rt2 <- reactive({ get_rt(A, list_audio_ind2()) })
  list_audio_rt3 <- reactive({ get_rt(A, list_audio_ind3()) })
  list_audio_rt4 <- reactive({ get_rt(A, list_audio_ind4()) })
  
  #get the full database - textarea words
  list_visual_data1 <- reactive({ V[list_visual_ind1(),] })
  list_visual_data2 <- reactive({ V[list_visual_ind2(),] })
  list_visual_data3 <- reactive({ V[list_visual_ind3(),] })
  list_visual_data4 <- reactive({ V[list_visual_ind4(),] })
  list_audio_data1 <- reactive({ A[list_audio_ind1(),] })
  list_audio_data2 <- reactive({ A[list_audio_ind2(),] })
  list_audio_data3 <- reactive({ A[list_audio_ind3(),] })
  list_audio_data4 <- reactive({ A[list_audio_ind4(),] })
  
  #get the index of missing textarea words in the lexical database
  list_visual_ind1_missing <- reactive({ get_missing(V, list_word1()) })
  list_visual_ind2_missing <- reactive({ get_missing(V, list_word2()) })
  list_visual_ind3_missing <- reactive({ get_missing(V, list_word3()) })
  list_visual_ind4_missing <- reactive({ get_missing(V, list_word4()) })
  list_audio_ind1_missing <- reactive({ get_missing(A, list_word1()) })
  list_audio_ind2_missing <- reactive({ get_missing(A, list_word2()) })
  list_audio_ind3_missing <- reactive({ get_missing(A, list_word3()) })
  list_audio_ind4_missing <- reactive({ get_missing(A, list_word4()) })
  
  
  #LIST - plot from lists of words
  output$list_output_plots <- renderPlot({
    #display the boxplot
    if (length(list_visual_ind1())!=0 || length(list_visual_ind2())!=0 || length(list_visual_ind3())!=0 || length(list_visual_ind4())!=0
        || length(list_audio_ind1())!=0 || length(list_audio_ind2())!=0 || length(list_audio_ind3())!=0 || length(list_audio_ind4())!=0){
      boxplot(list_visual_rt1(), list_visual_rt2(), list_visual_rt3(), list_visual_rt4(),
              list_audio_rt1(), list_audio_rt2(), list_audio_rt3(), list_audio_rt4(),
              names=c("Visual 1", "Visual 2", "Visual 3", "Visual 4",
                      "Audio 1", "Audio 2", "Audio 3", "Audio 4"), ylab="RT (in ms)",
              col=c("springgreen3","slategray3","tomato2","wheat3",
                    "springgreen3","slategray3","tomato2","wheat3"))
    }
  })
  
  
  #LIST - summary of basic statistics
  output$list_visual_output_summary_list1 <- renderPrint({ summary(list_visual_rt1()) })
  output$list_visual_output_summary_list2 <- renderPrint({ summary(list_visual_rt2()) })
  output$list_visual_output_summary_list3 <- renderPrint({ summary(list_visual_rt3()) })
  output$list_visual_output_summary_list4 <- renderPrint({ summary(list_visual_rt4()) })
  output$list_audio_output_summary_list1 <- renderPrint({ summary(list_audio_rt1()) })
  output$list_audio_output_summary_list2 <- renderPrint({ summary(list_audio_rt2()) })
  output$list_audio_output_summary_list3 <- renderPrint({ summary(list_audio_rt3()) })
  output$list_audio_output_summary_list4 <- renderPrint({ summary(list_audio_rt4()) })
  
  #basic anova
  output$list_visual_output_summary_anova <- renderPrint({
    lists_visual = c(rep(1,length(list_visual_ind1())), rep(2,length(list_visual_ind2())), rep(3,length(list_visual_ind3())), rep(4,length(list_visual_ind4()))) #create groups
    rts_visual = c(list_visual_rt1(), list_visual_rt2(), list_visual_rt3(), list_visual_rt4()) #create corresponding RTs
    anova(lm(rts_visual ~ lists_visual)) #compute and display
  })
  output$list_audio_output_summary_anova <- renderPrint({
    lists_audio = c(rep(1,length(list_audio_ind1())), rep(2,length(list_audio_ind2())), rep(3,length(list_audio_ind3())), rep(4,length(list_audio_ind4()))) #create groups
    rts_audio = c(list_audio_rt1(), list_audio_rt2(), list_audio_rt3(), list_audio_rt4()) #create corresponding RTs
    anova(lm(rts_audio ~ lists_audio)) #compute and display
  })
  
  
  
  
  #LIST - words may be missing
  output$list_output_missing <- renderUI({
    
    #get the textarea word lists (does not accept "list_word1[list_ind1_missing()]")
    w1 = list_word1()
    w2 = list_word2()
    w3 = list_word3()
    w4 = list_word4()
    
    #display the missing words
    fluidRow(
      tags$div(tags$strong("List 1: "), w1[list_visual_ind1_missing()]),
      tags$div(tags$strong("List 2: "), w2[list_visual_ind2_missing()]),
      tags$div(tags$strong("List 3: "), w3[list_visual_ind3_missing()]),
      tags$div(tags$strong("List 4: "), w4[list_visual_ind4_missing()]))
  })
  
  
  #LIST - tables from lists of words
  output$list1_visual_output_table <- renderDataTable({list_visual_data1()}, options = list(searching=FALSE))
  output$list1_audio_output_table <- renderDataTable({list_audio_data1()}, options = list(searching=FALSE))
  output$list2_visual_output_table <- renderDataTable({list_visual_data2()}, options = list(searching=FALSE))
  output$list2_audio_output_table <- renderDataTable({list_audio_data2()}, options = list(searching=FALSE))
  output$list3_visual_output_table <- renderDataTable({list_visual_data3()}, options = list(searching=FALSE))
  output$list3_audio_output_table <- renderDataTable({list_audio_data3()}, options = list(searching=FALSE))
  output$list4_visual_output_table <- renderDataTable({list_visual_data4()}, options = list(searching=FALSE))
  output$list4_audio_output_table <- renderDataTable({list_audio_data4()}, options = list(searching=FALSE))
  
  
  
  #LIST - export data
  output$list_export_data <- downloadHandler(
    
    filename = "output.csv",
    content = function(file) {
      
      #append the 4 datasets
      list_visual_data = rbind(
        data.frame(list = rep("VIS1",nrow(list_visual_data1())), list_visual_data1()),
        data.frame(list = rep("VIS2",nrow(list_visual_data2())), list_visual_data2()),
        data.frame(list = rep("VIS3",nrow(list_visual_data3())), list_visual_data3()),
        data.frame(list = rep("VIS4",nrow(list_visual_data4())), list_visual_data4()),
        data.frame(list = rep("AUD1",nrow(list_audio_data1())), list_audio_data1()),
        data.frame(list = rep("AUD2",nrow(list_audio_data2())), list_audio_data2()),
        data.frame(list = rep("AUD3",nrow(list_audio_data3())), list_audio_data3()),
        data.frame(list = rep("AUD4",nrow(list_audio_data4())), list_audio_data4())
        )
      
      write.csv(list_visual_data, file)}
  )
  
  
  #LIST - load example based on the smaller database
  observeEvent(input$list_load_example, {
    updateTextInput(session, "stim_list1", value = as.character(AUDIO$SPELLING[(1:nrow(AUDIO)) %in% sample(1:nrow(AUDIO),100) & AUDIO$LEXICALITY == 'w']))
    updateTextInput(session, "stim_list2", value = as.character(AUDIO$SPELLING[(1:nrow(AUDIO)) %in% sample(1:nrow(AUDIO),100) & AUDIO$LEXICALITY == 'w']))
    updateTextInput(session, "stim_list3", value = as.character(AUDIO$SPELLING[(1:nrow(AUDIO)) %in% sample(1:nrow(AUDIO),100) & AUDIO$LEXICALITY == 'w']))
    updateTextInput(session, "stim_list4", value = as.character(AUDIO$SPELLING[(1:nrow(AUDIO)) %in% sample(1:nrow(AUDIO),100) & AUDIO$LEXICALITY == 'w']))
  })
  
  #LIST - clear lists
  observeEvent(input$list_clear_list, {
    updateTextInput(session, "stim_list1", value = "")
    updateTextInput(session, "stim_list2", value = "")
    updateTextInput(session, "stim_list3", value = "")
    updateTextInput(session, "stim_list4", value = "")
  })
  
  

  #output$list_output_summary_list1 <- renderPrint({ list_word1() })
  #output$list_output_summary_list2 <- renderPrint({ list_word1() })
  
  
  ### VARIABLES ##################################################################
  

  var_visual_data = reactive(V[ 
    (V$nblettres >= input$ID_slider_nblettres[1]) & (V$nblettres <= input$ID_slider_nblettres[2]) & !(is.na(V$nblettres)) & 
      (V$nbsyll >= input$ID_slider_nbsyll[1]) & (V$nbsyll <= input$ID_slider_nbsyll[2]) & !(is.na(V$nbsyll)), ])
  
  var_audio_data = reactive(A[ 
    (A$nblettres >= input$ID_slider_nblettres[1]) & (A$nblettres <= input$ID_slider_nblettres[2]) & !(is.na(A$nblettres)) & 
      (A$nbsyll >= input$ID_slider_nbsyll[1]) & (A$nbsyll <= input$ID_slider_nbsyll[2]) & !(is.na(A$nbsyll)), ])
  
  output$variable_table <- renderUI({
    
      fixedRow(
        column(4,
               h4("nblettres"),
               sliderInput("ID_slider_nblettres", "" ,
                           min=floor( min(V[, "nblettres"], na.rm=TRUE) ), 
                           max=ceiling( max(V[, "nblettres"], na.rm=TRUE)  ), 
                           value=c(floor( min(V[, "nblettres"], na.rm=TRUE) ), ceiling( max(V[,"nblettres"], na.rm=TRUE))))
        ),
        column(4,
               h4("nbsyll"),
               sliderInput("ID_slider_nbsyll", "" ,
                           min=floor( min(V[, "nbsyll"], na.rm=TRUE) ), 
                           max=ceiling( max(V[, "nbsyll"], na.rm=TRUE)  ), 
                           value=c(floor( min(V[, "nbsyll"], na.rm=TRUE) ), ceiling( max(V[,"nbsyll"], na.rm=TRUE))))
               )
      )
    
    })
  
  output$TEST_THINGS_FOR_DEBUGGING <- renderPrint({  
    var_visual_data()$SPELLING
  })
  
  output$var_export_visual <- downloadHandler(
    filename = "megalex_visual.csv",
    content = function(file) { write.csv(var_visual_data() , file) })
  
  output$var_export_audio <- downloadHandler(
    filename = "megalex_audio.csv",
    content = function(file) { write.csv(var_audio_data() , file)})
  
  


  ### RAW DATA ##################################################################
  #export data
  output$list_visual_export_all <- downloadHandler(
    filename = "megalex_visual.csv",
    content = function(file) { write.csv(V[V$LEXICALITY == 'w',], file)})

  output$list_audio_export_all <- downloadHandler(
    filename = "megalex_audio.csv",
    content = function(file) { write.csv(A[A$LEXICALITY == 'w',], file)})  
  
  
  
  } # end of server

# Run the application 
shinyApp(ui = ui, server = server)




