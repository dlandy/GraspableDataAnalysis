currentGMVersion = "2.5.12"

#' importGraspableJson
#' 
#' This simple script just loads a json data file, merges the trial data and events, and renames a few columns
#' It also twiddles things down to a single version of GM, and a single context ("experiment").  It does this only if you
#' set 'compress' to true, but that seems like the vastly most useful format by default.
#' 
#' @param fileName the name of a file locating a json project
#' @param version If any non-false value, data will be limited to versions of gm matching this string
#' @param limitExperiment If not false, limit to experiment contexts matching this value
#' @param compress If set to true, execute logic to collapse each interaction into a single row, glossing
#' diferences between touch, tap, release, etc events.
#' @param debug If true, does a number of sanity checks, like looking at the number of unique old states when collapsing
#' Comments
#' 
#' @return a tibble containing the data from the file, with the class "GM X", where X is the version 
#' specified (or the first version number to appear). 
#' @seealso 
#' @export
#' @examples
#' canvasSnippetFile <- system.file("extdata", "canvasSnippet.json", package = "GraspableDataAnalysis", mustWork = TRUE)
#' canvasSnippetData <- importGraspableJson(canvasSnippetFile, version = "2.5.12" )
#' 
importGraspableJson <- function(filename, version = F, limitExperiment = F, compress=T, debug=F){
  subtypeList <- c("font_size", "mode_change", "create", "create", "delete", "draw", "undo", "clear_all"
                   , "move", "inspect", "clone", "scrub", "redo", "auto-undo")
  safeHead <- function(ll){
    if(length(ll)==0){return("none")} else {return(head(ll, 1))}
  }
  safeTail <- function(ll){
    if(length(ll)==0){return("none")} else {return(tail(ll, 1))}
  }
  graspableJSON <- jsonlite::fromJSON(read_file(filename), simplifyDataFrame=T, flatten=T)
  gmData <- as.tibble(graspableJSON$data)
  trials <- as.tibble(graspableJSON$trials)
  
  graspableJSON <- as.tibble(merge(gmData, trials, by.x="trial_id", by.y="id"))
  
  graspableJSON$uniqueID <- graspableJSON$`_id.x`
  graspableJSON <- rename(graspableJSON
                          , "trialTime" = "time.y" 
                          , "trialTimeStamp" = "timestamp.y"
                            ) 
  #graspableJSON <- select(graspableJSON, -`_id`)
  
  if(version != F){
    graspableJSON <- graspableJSON %>% filter(gm_version %in% version)
    class(graspableJSON) <- append(paste("gm", version)
                                   , class(graspableJSON))
    
  } else {
    class(graspableJSON) <- append(paste("gm", graspableJSON$gm_version[1])
                                   , class(graspableJSON))
    
  }
  if(limitExperiment != F){
    graspableJSON <- graspableJSON %>% filter(experiment_id %in% limitExperiment)
  }
  
  # Collapse Rows of interaction ids down to single elements
  if(compress){
    i <<- 0
    graspableJSON$marker <- paste(graspableJSON$trial_id, graspableJSON$interaction_id)
    graspableJSON <- graspableJSON %>% split(.$marker)  %>%
                  map(function(d){
                      if(debug){
                        print(i)
                        i<<- i+1
                      }
                      e <- with(d, tibble(
                          context = safeHead(na.omit(experiment_id))
                        , canvasID = safeHead((na.omit(canvas_id)))
                        , elementID = safeHead(na.omit(el_id))
                        , interactionID = mean(interaction_id)
                        , uniqueID = safeHead(uniqueID)
                        , type = ifelse("interaction" %in% type, 'interaction', "event")
                        , subtype = safeHead(subtype[subtype %in% subtypeList])
                        , action = safeTail(na.omit(action))
                        , method = safeTail(na.omit(method))
                        , old_state = safeTail(na.omit(old_state))
                        , newState = safeTail(na.omit(new_state))
                        , duration = sum(na.omit(dur))
                        , elementType = safeHead(na.omit(el_type))
                        
                        
                        
                             ))
                     
                      if(debug){
                        e <- with(d,  mutate(e 
                        , oldStateDebugCount = length(na.omit(old_state))
                        , newStateDebugCount = length(na.omit(new_state))
                        , subtypeDebugCount = length(na.omit(subtype[subtype %in% subtypeList]))
                        , actionDebugCount = length(na.omit(action))
                        ))
                      }
                      e
                      
                      }) %>% bind_rows()

    graspableJSON <- graspableJSON %>% arrange(.$context, .$canvasID, .$interactionID)
  }
 
  graspableJSON
}

