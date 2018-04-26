currentGMVersion = "2.5.12"

#' importGraspableJson
#' 
#' This simple script just loads a json data file, merges the trial data and events, and renames a few columns
#' It also twiddles things down to a single version of GM, and a single context ("experiment").  It does this only if you
#' set 'compress' to true, but that seems like the vastly most useful format by default.
#' 
#' @param fileName the name of a file locating a json project
#' @param versions A vector of versions of Graspable math to include.  Data that are associated 
#' with a different version are excluded by default. If any non-false value, data will be limited to versions of gm matching this string
#' @param contexts A vector of contexts to include. GM is used in many contexts, including experiments, the canvas, and so on.  If set to F, include all contexts.
#' @param compress If set to true, execute logic to collapse each interaction into a single row, glossing
#' diferences between touch, tap, release, etc events.
#' @param debug If true, does a number of sanity checks, like looking at the number of unique old states when collapsing
#' Comments
#' 
#' @return a tibble containing the data from the file, with the class "GM X", where X is the version 
#' specified (or the first version number to appear). 
#' @description Graspable data is stored in JSON [describe what the data format is like, and what compressing is.]
#' @seealso 
#' @export
#' @examples
#' canvasSnippetFile <- system.file("extdata", "canvasSnippet.json", package = "GraspableDataAnalysis", mustWork = TRUE)
#' canvasSnippetData <- importGraspableJson(canvasSnippetFile, versions = c("2.5.12" ))
#' 
importGraspableJson <- function(filename, versions = F, contexts = F, compress=T, debug=F){
  subtypeList <- c("font_size", "mode_change", "create", "create", "delete", "draw", "undo", "clear_all"
                   , "move", "inspect", "clone", "scrub", "redo", "auto-undo", "math")
  safeHead <- function(ll){
    nilVal <- "none"
    if(length(intersect(class(ll), c("integer", "numeric"))) > 0){
      nilVal <- NA
    }
    if(length(ll)==0){return(nilVal)} else {return(head(ll, 1))}
  }
  safeTail <- function(ll){
    nilVal <- "none"
    if(length(intersect(class(ll), c("integer", "numeric"))) > 0){
      nilVal <- NA
    }
    if(length(ll)==0){return(nilVal)} else {return(tail(ll, 1))}
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
  
  if(versions != F){
    graspableJSON <- graspableJSON %>% filter(gm_version %in% versions)
    class(graspableJSON) <- append(paste("gm", versions)
                                   , class(graspableJSON))
    
  } else {
    class(graspableJSON) <- append(paste("gm", graspableJSON$gm_version[1])
                                   , class(graspableJSON))
    
  }
  if(contexts != F){
    graspableJSON <- graspableJSON %>% filter(experiment_id %in% contexts)
  }
  
  # Collapse Rows of interaction ids down to single elements
  if(compress){
    i <<- 0
    graspableJSON$marker <- paste(graspableJSON$trial_id, graspableJSON$interaction_id)
    if(debug){print("Building Compressed Table")}
    
    graspableJSON <- graspableJSON %>% split(.$marker)  %>%
                  map(function(d){
                      if(debug){
                        if(i%% 50 == 0){print(i)}
                        i<<- i+1
                      }
                      e <- with(d, tibble(
                          context = safeHead(na.omit(experiment_id))
                        , canvasID = safeHead((na.omit(canvas_id)))
                        , elementID = safeHead(na.omit(el_id))
                        , interactionID = mean(interaction_id)
                        , uniqueID = safeHead(uniqueID)
                        , type = ifelse("interaction" %in% type, 'interaction', "event")
                        , action = safeTail(na.omit(action))
                        , subtype = safeHead(subtype[subtype %in% subtypeList])
                        , method = safeTail(na.omit(method))
                        , oldState = safeTail(na.omit(old_state))
                        , newState = safeTail(na.omit(new_state))
                        , duration = sum(na.omit(dur))
                        , elementType = safeHead(na.omit(el_type))
                        , exprXInitial = safeHead(na.omit(expr_x))
                        , exprYInitial = safeHead(na.omit(expr_y))
                        , exprXFinal = safeTail(na.omit(expr_x))
                        , exprYFinal = safeTail(na.omit(expr_y))
                        , exprWidthInitial = safeHead(na.omit(expr_width))
                        , exprHeightInitial = safeHead(na.omit(expr_height))
                        , exprWidthFinal = safeTail(na.omit(expr_width))
                        , exprHeightFinal = safeTail(na.omit(expr_height))
                        , symXInitial = safeHead(na.omit(sym_x))
                        , symYInitial = safeHead(na.omit(sym_y))
                        , selXInitial = safeHead(na.omit(sel_x))
                        , selYInitial = safeHead(na.omit(sel_y))
                        , symXFinal = safeTail(na.omit(sym_x))
                        , symYFinal = safeTail(na.omit(sym_y))
                        , selXFinal = safeTail(na.omit(sel_x))
                        , selYFinal = safeTail(na.omit(sel_y))
                        , timeStart = min(time.x, na.omit=T)
                        , timeFinal = max(time.x, na.omit=T)
                        , xInitial = NA
                        , yInitial = NA
                             ))
                     
                      
                      
                      if("move" %in% d$subtype){
                        startingPlace <- jsonlite::fromJSON(safeHead(na.omit(d$old_state)))
                        e$xInitial <- startingPlace$x
                        e$yInitial <- startingPlace$y
                        endingPlace <- jsonlite::fromJSON(safeHead(na.omit(d$new_state)))
                        e$xFinal <- endingPlace$x
                        e$yFinal <- endingPlace$y
#                        print(startingPlace)
                      }
                      
                      
                      
                      if(debug){
                        e <- with(d,  mutate(e 
                        , oldStateDebugCount = length(na.omit(old_state))
                        , newStateDebugCount = length(na.omit(new_state))
                        , subtypeDebugCount = length(na.omit(subtype[subtype %in% subtypeList]))
                        , actionDebugCount = length(na.omit(action))
                        , exprXCount = length(unique(na.omit(expr_x)))
                        , exprYCount = length(unique(na.omit(expr_y)))
                        , exprWidthCount = length(unique(na.omit(expr_width)))
                        , exprHeightCount = length(unique(na.omit(expr_height)))
                        , symXCount = length(unique(na.omit(sym_x)))
                        , symYCount = length(unique(na.omit(sym_y)))
                        , selXCount = length(unique(na.omit(sel_x)))
                        , selYCount = length(unique(na.omit(sel_y)))
                        ))
                      }
                      return(e)
                      
                      }) %>% bind_rows()

    graspableJSON <- graspableJSON %>% arrange(.$context, .$canvasID, .$interactionID)
    
    i <<- 0
    if(debug){print("Propogating absolute locations")}
    graspableJSON <- graspableJSON %>% split(.$elementID)  %>%
      map(function(d){
        if(debug){
          print(i)
          i<<- i+1
        }
        currentX <- safeHead(d$xInitial)
        currentY <- safeHead(d$yInitial)
        for(i in length(d$xFinal)){
          if(length(na.omit(d$xInitial))==0){
            print("yes")
            d$xInitial <- currentX
            d$yInitial <- currentY
          } else {
            print(d$xInitial)
            currentX <- na.omit(d$xInitial)
            currentY <- na.omit(d$yInitial)
          }
        }
        return(d)
      }) %>% bind_rows()
  }
 
  graspableJSON
}

