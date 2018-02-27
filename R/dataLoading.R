currentGMVersion = "2.5.12"

#' importGraspableJson
#' @param fileName a vector of stimuli, between 0 and inf
#' Comments
#' 
#' @return A list of Tibbles
#' @seealso 
#' @export
#' @examples
#' dataList <- importGraspableJson("canvasSnippet.json")
#' 
importGraspableJson <- function(filename, singleVersionOnly = currentGMVersion, limitExperiment = F){
  graspableJSON <- jsonlite::fromJSON(read_file(filename), simplifyDataFrame=T, flatten=T)
  trials <- as.tibble(graspableJSON$trials)
  graspableJSON <- as.tibble(merge(data, trials, by.x="trial_id", by.y="id"))
  
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
    
}
