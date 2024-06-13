library(shiny)
library(stringr)
library(DT)

# Define UI for application
ui <- fluidPage(

  fileInput('picInput', label = "DROP A MF PICTUE UP IN THIS BIT"),
  
  actionButton('getNamesButton', 'GET NAMES'),
  dataTableOutput('winRateTable')
  

)

# Define server logic 
server <- function(input, output) {
  
  source("Funcs.R")
  
  observeEvent(input$getNamesButton, {
    # pic_filepath = input$picInput$datapath
    
    pic_filepath = "www/closer.JPG"
    
    print(pic_filepath)
    
    vision_response = gemini_vision("Can you give me all of the character names and realms from this picture.", image = pic_filepath)
    
    all_names = tolower(trimws(str_match_all(string = vision_response, pattern = "\\*(.*?)-")[[1]][,2])) %>%
      str_replace_all(pattern = "\\*\\*", replacement = "")
    
    all_realms = tolower(trimws(str_match_all(string = vision_response, pattern = "-(.*?)\\n")[[1]][,2])) %>%
      str_replace_all(pattern = " ", replacement = "-") %>%
      str_replace_all(pattern = "\\*\\*", replacement = "") %>%
      str_replace_all(pattern = "'", replacement = "")
    
    win_rates = c()
    ratings_2v2 = c()
    ratings_3v3 = c()
    ratings_rbg = c()
    
    for(i in 1:length(all_names)){
     win_rate = GetCharacterWinrate(all_names[i], all_realms[i])
     
     rating_2v2 = GetCharacterRatings(charactername = all_names[i], server = all_realms[i], bracket = "2v2")
     rating_3v3 = GetCharacterRatings(charactername = all_names[i], server = all_realms[i], bracket = "3v3")
     rating_rbg = GetCharacterRatings(charactername = all_names[i], server = all_realms[i], bracket = "rbg")
     
     win_rates = c(win_rates, win_rate)
     
     ratings_2v2 = c(ratings_2v2, rating_2v2)
     ratings_3v3 = c(ratings_3v3, rating_3v3)
     ratings_rbg = c(ratings_rbg, rating_rbg)
     
     
     print(paste0(i, ", completed out of: ", length(all_names)))
    }
    
    print(paste0("2s Ratings ", ratings_2v2))
    print(paste0("3s Ratings ", ratings_3v3))
    print(paste0("RBG Ratings ", ratings_rbg))
    
    df_display = data.frame(
      "Character Name" = all_names,
      "Character Realm" = all_realms,
      "Win Rate" = win_rates,
      "2v2 Rating" = ratings_2v2,
      "3v3 Rating" = ratings_3v3,
      "RBG Rating" = ratings_rbg
    )
    
    output$winRateTable = renderDataTable({
      datatable(df_display)
    })
    
    
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
