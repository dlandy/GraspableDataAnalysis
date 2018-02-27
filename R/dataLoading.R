currentGMVersion = "2.5.12"

#' importGraspableJson
#' This simple script just loads a json data file, merges the trial data and events, and renames a few columns
#' Optional, it also twiddles things down to a single version of GM, and a single context ("experiment").  
#' 
#' @param fileName the name of a file locating a json project
#' @param version If any non-false value, data will be limited to versions of gm matching this string
#' @param limitExperiment If not false, limit to experiment contexts matching this value
#' Comments
#' 
#' @return a tibble containing the data from the file, with the class "GM X", where X is the version 
#' specified (or the first version number to appear)
#' @seealso 
#' @export
#' @examples
#' canvasSnippetFile <- system.file("extdata", "canvasSnippet.json", package = "GraspableDataAnalysis", mustWork = TRUE)
#' canvasSnippetData <- importGraspableJson(canvasSnippetFile, version = "2.5.12" )
#' 
importGraspableJson <- function(filename, version = F, limitExperiment = F){
  graspableJSON <- jsonlite::fromJSON(read_file(filename), simplifyDataFrame=T, flatten=T)
  gmData <- as.tibble(graspableJSON$data)
  trials <- as.tibble(graspableJSON$trials)
  
  graspableJSON <- as.tibble(merge(gmData, trials, by.x="trial_id", by.y="id"))
  
  graspableJSON <- rename(graspableJSON
                          , "trialTime" = "time.y" 
                          , "trialTimeStamp" = "timestamp.y"
  )
  
  
  if(version != F){
    graspableJSON <- graspableJSON %>% filter(gm_version==version)
    class(graspableJSON) <- append(paste("gm", version)
                                   , class(graspableJSON))
    
  } else {
    class(graspableJSON) <- append(paste("gm", graspableJSON$gm_version[1])
                                   , class(graspableJSON))
    
  }
  if(limitExperiment != F){
    graspableJSON <- graspableJSON %>% filter(experiment_id %in% limitExperiment)
  }
  graspableJSON
}
