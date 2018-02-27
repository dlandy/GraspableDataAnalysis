currentGMVersion = "2.5.12"

#' importGraspableJson
#' @param fileName a vector of stimuli, between 0 and inf
#' Comments
#' 
#' @return A list of Tibbles
#' @seealso 
#' @export
#' @examples
#' canvasSnippetFile <- system.file("extdata", "canvasSnippet.json", package = "GraspableDataAnalysis", mustWork = TRUE)
#' canvasSnippetData <- importGraspableJson(canvasSnippetFile, singleVersionOnly = "2.5.12" )
#' 
importGraspableJson <- function(filename, singleVersionOnly = F, limitExperiment = F){
  graspableJSON <- jsonlite::fromJSON(read_file(filename), simplifyDataFrame=T, flatten=T)
  gmData <- as.tibble(graspableJSON$data)
  trials <- as.tibble(graspableJSON$trials)
  
  graspableJSON <- as.tibble(merge(gmData, trials, by.x="trial_id", by.y="id"))
  
  graspableJSON <- rename(graspableJSON
                          , "trialTime" = "time.y" 
                          , "trialTimeStamp" = "timestamp.y"
  )
  
  
  if(singleVersionOnly != F){
    graspableJSON <- graspableJSON %>% filter(gm_version==singleVersionOnly)
  }
  if(limitExperiment != F){
    graspableJSON <- graspableJSON %>% filter(experiment_id %in% limitExperiment)
  }
  graspableJSON
}
