
get_dataset <- function(path_folder,dataset_names){
  data <- list()
  for(dataset in dataset_names){
    #browser()
    path <- paste(path_folder,dataset,"/",sep="",collapse="")
    evt_files <- paste(path,list.files(path = path, pattern = ".Rds", recursive = TRUE),sep="")
    evt_dataset <- sapply(evt_files, FUN=readRDS, simplify = FALSE, USE.NAMES = TRUE)
    names(evt_dataset) <- stringr::str_remove(evt_files, path)
    names(evt_dataset) <- stringr::str_remove(names(evt_dataset), ".Rds")

    data[[dataset]] <- evt_dataset
  }
  return(data)
}
