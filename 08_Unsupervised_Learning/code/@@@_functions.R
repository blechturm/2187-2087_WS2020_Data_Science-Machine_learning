#Function to get the local dropbox path
dropboxpath <- function(subfolder=""){
  
  
  if (file.exists(file.path(Sys.getenv("APPDATA"),
                            "Dropbox",
                            "info.json"))) {
    
    file.path(jsonlite::read_json(file.path(Sys.getenv("APPDATA"),
                                            "Dropbox",
                                            "info.json"))$personal$path,
              subfolder)
    
  } else if (file.exists(file.path(Sys.getenv("LOCALAPPDATA"),
                                   "Dropbox",
                                   "info.json"))) {
    
    file.path(jsonlite::read_json(file.path(Sys.getenv("LOCALAPPDATA"),
                                            "Dropbox",
                                            "info.json"))$personal$path,
              subfolder)
    
  } else if (file.exists(file.path("~/.dropbox/info.json"))){
    
    file.path(jsonlite::read_json(file.path("~/.dropbox/info.json"))$personal$path, subfolder)
  } else {"no_dropbox_installation_found"}
}