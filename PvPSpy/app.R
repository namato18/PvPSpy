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
    
    pic_filepath = "C:/Users/xbox/Pictures/closer.JPG"
    
    print(pic_filepath)
    
    vision_response = gemini_vision("Can you give me all of the character names and realms from this picture.", image = pic_filepath)
    
    all_names = tolower(trimws(str_match_all(string = vision_response, pattern = "\\*(.*?)-")[[1]][,2])) %>%
      str_replace_all(pattern = "\\*\\*", replacement = "")
    
    all_realms = tolower(trimws(str_match_all(string = vision_response, pattern = "-(.*?)\\n")[[1]][,2])) %>%
      str_replace_all(pattern = " ", replacement = "-") %>%
      str_replace_all(pattern = "\\*\\*", replacement = "") %>%
      str_replace_all(pattern = "'", replacement = "")
    
    win_rates = c()
    for(i in 1:length(all_names)){
     win_rate = GetCharacterWinrate(all_names[i], all_realms[i])
     
     win_rates = c(win_rates, win_rate)
     
     print(paste0(i, ", completed out of: ", length(all_names)))
    }
    
    df_display = data.frame(
      "Character Name" = all_names,
      "Character Realm" = all_realms,
      "Win Rate" = win_rates
    )
    
    output$winRateTable = renderDataTable({
      datatable(df_display)
    })
    
    
    
  })


}

# Run the application 
shinyApp(ui = ui, server = server)
