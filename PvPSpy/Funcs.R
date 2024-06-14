library(httr)
library(jsonlite)
library(base64enc)
library(dplyr)
library(tidyr)

access_token = system("curl -u 6ab02cc1a1c140f0bd213d0b27e8c74c:VTrUzsJw0XDmKfJvM1QIXpn1QC1Hs96e -d grant_type=client_credentials https://us.battle.net/oauth/token",
                      intern = TRUE)
access_token = jsonlite::fromJSON(access_token[4], flatten = TRUE)
access_token = access_token$access_token

api_key <- Sys.setenv(GEMINI_API_KEY = "AIzaSyCFCKa8CM0c_khCk8qBtGZWInPO2pFvKak")
gemini <- function(prompt, 
                   temperature=0.5,
                   max_output_tokens=1024,
                   api_key=Sys.getenv("GEMINI_API_KEY"),
                   model = "gemini-1.0-pro") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(text = prompt)
        )),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}


chat_gemini <- function(prompt, 
                        temperature=0.5,
                        api_key=Sys.getenv("GEMINI_API_KEY"),
                        model="gemini-1.0-pro") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  # Add new message
  chatHistory <<- append(chatHistory, list(list(role = 'user', 
                                                parts = list(
                                                  list(text = prompt)
                                                ))))
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    body = toJSON(list(
      contents = chatHistory,
      generationConfig = list(
        temperature = temperature
      )
    ),  auto_unbox = T))
  
  if(response$status_code>200) {
    chatHistory <<- chatHistory[-length(chatHistory)]
    stop(paste("Status Code - ", response$status_code))
  } else {
    answer <- content(response)$candidates[[1]]$content$parts[[1]]$text
    chatHistory <<- append(chatHistory, list(list(role = 'model', 
                                                  parts = list(list(text = answer)))))
  }
  
  return(answer)
  
}

# Function
gemini_vision <- function(prompt, 
                          image,
                          temperature=0.1,
                          max_output_tokens=4096,
                          api_key=Sys.getenv("GEMINI_API_KEY"),
                          model = "gemini-1.5-pro-latest") {
  
  if(nchar(api_key)<1) {
    api_key <- readline("Paste your API key here: ")
    Sys.setenv(GEMINI_API_KEY = api_key)
  }
  
  model_query <- paste0(model, ":generateContent")
  
  response <- POST(
    url = paste0("https://generativelanguage.googleapis.com/v1beta/models/", model_query),
    query = list(key = api_key),
    content_type_json(),
    encode = "json",
    body = list(
      contents = list(
        parts = list(
          list(
            text = prompt
          ),
          list(
            inlineData = list(
              mimeType = "image/png",
              data = base64encode(image)
            )
          )
        )
      ),
      generationConfig = list(
        temperature = temperature,
        maxOutputTokens = max_output_tokens
      )
    )
  )
  
  if(response$status_code>200) {
    stop(paste("Error - ", content(response)$error$message))
  }
  
  candidates <- content(response)$candidates
  outputs <- unlist(lapply(candidates, function(candidate) candidate$content$parts))
  
  return(outputs)
  
}

######################################
######################################
######################################
######################################


GetPvPSeason <- function(){
  response = GET(paste0("https://us.api.blizzard.com/data/wow/pvp-season/index?namespace=dynamic-us&locale=en_US&access_token=",access_token))
  
  x = fromJSON(rawToChar(response$content))
  
  return(x$current_season$id)
}

season_id = GetPvPSeason()

######################################
######################################
######################################
######################################

GetCharacterWinrate <- function(charactername, server) {
  # server = 'azralon'
  # charactername = 'arcontt'
  
  url = URLencode(paste0("https://us.api.blizzard.com/profile/wow/character/",server,"/",charactername,"/pvp-summary?namespace=profile-us&locale=en_US&access_token=",access_token))
  
  response = GET(url)
  
  if(response$status_code != 200){
    return(NA)
  }
  
  x = fromJSON(rawToChar(response$content))
  
  df = x$pvp_map_statistics %>%
    unnest(world_map, names_sep = "_") %>%
    unnest(match_statistics, names_sep = "_")
  
  win_rate = paste0(round(sum(df$match_statistics_won) / sum(df$match_statistics_played) * 100, 1), "%")
  
  return(win_rate)
}

######################################
######################################
######################################
######################################

# /profile/wow/character/{realmSlug}/{characterName}/pvp-bracket/{pvpBracket}

GetCharacterRatings <- function(charactername, server, bracket) {
  #server = 'tichondrius'
  #charactername = 'yach'
  #bracket = '2v2'
  
  url = URLencode(paste0("https://us.api.blizzard.com/profile/wow/character/", server,"/",charactername, "/pvp-bracket/", bracket, "?namespace=profile-us&locale=en_US&access_token=",access_token))
  
  
  response = GET(url)
  
  if(response$status_code != 200){
    return(NA)
  }
  
  x = fromJSON(rawToChar(response$content))
  
  rating = x$rating
  
  return(rating)
}

######################################
######################################
######################################
######################################

# /data/wow/pvp-season/{pvpSeasonId}/pvp-leaderboard/index

GetLeaderboardIndex <- function(charactername, server, bracket) {
  #server = 'tichondrius'
  #charactername = 'yach'
  #bracket = '2v2'
  
  url = URLencode(paste0("https://us.api.blizzard.com/data/wow/pvp-season/", season_id,"/pvp-leaderboard/index?namespace=dynamic-us&locale=en_US&access_token=",access_token))
  
  
  response = GET(url)
  
  if(response$status_code != 200){
    return(NA)
  }
  
  x = fromJSON(rawToChar(response$content))
  
  rating = x$rating
  
  return(rating)
}

######################################
######################################
######################################
######################################
