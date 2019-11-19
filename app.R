## Knowledge hacking - text mining for language learning! ##

setwd("~/Projects/knowledge_hacking")

source("src/tools/load_app_libraries.R")
load_app_libraries()

ud_model <- udpipe_load_model("src/tools/spanish-ancora-ud-2.3-181115.udpipe")

source("src/data/runTextAnalysis.R")

googleSheet_embed_link <- "https://docs.google.com/spreadsheets/d/1uClBsSLJD2O0R-GcwIfNhMYsJYr2KPg1QW__KWUYLLg"

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Knowledge Hacking",
                  titleWidth = 300),
  dashboardSidebar(
    width = 300,
    rclipboardSetup(),
    sidebarMenu(
      menuItem("Load text", tabName = "load_text"),
      menuItem("Learning list", tabName = "learning_list")
    ),
    selectizeInput("file1", 'Choose .txt file:',
                   choices = as.character(list.files("src/data/books")),
                   selected = as.character(list.files("src/data/books"))[1]),
    selectizeInput("word_type", 'Choose word type:',
                   choices = c("ADJ","ADV","NOUN","VERB")),
    actionButton("do", "Let's learn some words")
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
                              .content-wrapper, .right-side {
                              background-color: #ffffff;
                              }
                              '))
      ),
    tabItems(
      tabItem("load_text",
              fluidRow(
                column(8,
                       div(style = "padding: 10px 10px;",
                           tags$script(HTML(
                             "$(document).on('click', '#canvas', function() {",
                             'word = document.getElementById("wcSpan").innerHTML;',
                             "Shiny.onInputChange('selected_word', word);",
                             "});"
                           )),
                           withLoader(wordcloud2Output("wordcloud", width = "90%"),
                                      type="image", proxy.height = "600px",
                                      loader="https://static.skillshare.com/uploads/project/d6af5b5bb8f3f6b3bc4ac5d63675e527/467a53df")
                       )
                ),
                column(4,
                       div(style = "font-size: 10px; padding: 14px 0px; margin:10%",
                           h4(htmlOutput("selectedWord")),
                           br(),
                           uiOutput("copyButton")
                       )
              ))),
      tabItem("learning_list",
              fluidRow(
                htmlOutput("googleSheet")
              ))
      )
    )
  )

  
  
server <- function(input, output, session) {
  
    output$googleSheet <- renderUI({
      tags$iframe(id = "googleSheet",
                  src = googleSheet_embed_link,
                  width = "1200",
                  height = "800",
                  frameborder = 0,
                  marginheight = 0)
      })
    
    fileSelect <- eventReactive(input$do, {
      paste0("src/data/books/",input$file1)
    })

    
    wordSelect <- eventReactive(input$selected_word, {
      input$selected_word
    })
    
    wordCloud <- eventReactive(input$do, {
      cat(paste0("Processing ",fileSelect(),"...\n"))
      x <- runTextAnalysis(fileSelect(), ud_model, input$word_type)
      return(x)
    })
    
    output$wordcloud <- renderWordcloud2({
      
        x <- wordCloud()[[1]]
        return(x)
        
      })
    
    output$selectedWord <- renderUI({
      
      tmp <- paste0('Word: ',sub("\\:.*", "", wordSelect()),'<br><br>',
                        '<a href="https://www.wordreference.com/es/en/translation.asp?spen=',sub("\\:.*", "", wordSelect()),'" target="_blank">Translation</a><br><br>',
                        '<a href="https://www.wordreference.com/sinonimos/',sub("\\:.*", "", wordSelect()),'" target="_blank">Synonyms</a>','<br><br>',
                        'Example from text: ',
                    wordCloud()[[2]] %>%
                      filter(lemma == sub("\\:.*", "", wordSelect())) %>%
                      select(sentence) %>% sample() %>% head(1) %>% pull())
      
      HTML(tmp)
      
    })
    
    # Add clipboard buttons
    output$copyButton <- renderUI({
      
      if(!is.null(input$selected_word)) {
        
        rclipButton("clipbtn", "Copy word for learning list", sub("\\:.*", "", wordSelect()),
                    icon("clipboard"))
        
      } else { NULL }
      
    })
      
}

# Run the application 
shinyApp(ui = ui, server = server)



