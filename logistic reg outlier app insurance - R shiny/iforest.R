library(ggplot2)
library(solitude)
library(Gmisc)
isolationForest <- function(dataset, ...){
  
  
  
  columnNames  <- colnames(dataset)
  
  responseName <- columnNames[[1]]
  
  while(deparse(substitute(responseName)) %in% columnNames){
    
    responseName <- sample(c(letters, LETTERS), 20, replace = TRUE)
    
  }
  
  
  
  arguments <- list(...)
  
  arg_names <- names(arguments)
  
  
  
  fixed_names <- c("dependent.variable.name"
                   
                   , "data"
                   
                   , "mtry"
                   
                   , "min.node.size"
                   
                   , "splitrule"
                   
                   , "num.random.splits"
                   
  )
  
  if(any(fixed_names %in% arg_names)){
    
    stop(
      
      paste0("These optional arguments of ranger::ranger should not be specified: "
             
             , toString(fixed_names)
             
      )
      
    )
    
  }
  
  
  
  if("seed" %in% names(arguments)){
    
    seed <- arguments[["seed"]]
    
  } else {
    
    seed <- sample.int(1e5, 1)
    
  }
  
  
  
  if(!("replace" %in% arg_names)){
    
    arguments[["replace"]] <- FALSE
    
  }
  
  
  
  if(!("sample.fraction" %in% arg_names)){
    
    arguments[["sample.fraction"]] <- 1
    
  }
  
  
  
  if(!("respect.unordered.factors" %in% arg_names)){
    
    arguments[["respect.unordered.factors"]] <- "partition"
    
  }
  
  
  
  set.seed(seed)
  
  dataset[[deparse(substitute(responseName))]] <- sample.int(nrow(dataset))
  
  
  
  # build a isolation forest
  
  iso <- fastDoCall(ranger::ranger
                    
                    , c(
                      
                      list(
                        
                        dependent.variable.name     = deparse(substitute(responseName))
                        
                        , data                      = dataset
                        
                        , mtry                      = 1
                        
                        , min.node.size             = 1
                        
                        , splitrule                 = "extratrees"
                        
                        , num.random.splits         = 1
                        
                      )
                      
                      , arguments
                      
                    )
                    
  )
  
  
  
  return(structure(list(forest = iso), class = "solitude"))
  
}

predict.solitude <- function(object
                             
                             , data
                             
                             , type       = "anomaly_score"
                             
                             , aggregator = "median"
                             
                             , ...
                             
){
  
  
  
  if(!(type %in% c("anomaly_score", "depth_corrected"))){
    
    stop("type has to be among: anomaly_score, depth_corrected")
    
  }
  
  
  
  res <- switch(type
                
                , depth_corrected = depth_corrected(object, data, ...)
                
                , anomaly_score   = anomaly_score(object, data, aggregator, ...)
                
  )
  
  
  
  return(res)
  
}



#' @name depth_corrected

#' @title depth_corrected

#' @description depth_corrected

#' @param object isolation forest model

#' @param newdata dataframe to predict

#' @param ... Arguments to be passed to future.apply::future_lapply

depth_corrected <- function(object, newdata, ...){
  
  
  
  args_future <- list(...)
  
  
  
  num_trees <- object[["forest"]][["num.trees"]]
  
  
  
  tnm <- predict(object[["forest"]]
                 
                 , data = newdata
                 
                 , type = "terminalNodes"
                 
  )[["predictions"]] + 1L
  
  
  
  
  
  get_corrected_depths <- function(x){
    
    depths <- c(
      
      depth_terminalNodes(ranger::treeInfo(object[["forest"]], x))
      
      , average_path_length(table(tnm[, x]))
      
    )
    
    
    
    tapply(depths, names(depths), sum)
    
  }
  
  
  
  corrected_depths <-
    
    Reduce(
      
      cbind
      
      , fastDoCall(
        
        future.apply::future_lapply
        
        , c(
          
          list(1:num_trees
               
               , function(x) as.numeric(get_corrected_depths(x)[as.character(tnm[, x])])
               
          )
          
          , args_future
          
        )
        
      )
      
    )
  
  
  
  return(corrected_depths)
  
}



#' @name anomaly_score

#' @title anomaly_score

#' @description anomaly_score

#' @param object isolation forest model

#' @param newdata dataframe to predict

#' @param aggregator aggregator

#' @param ... Arguments to be passed to future.apply::future_lapply in

#'   'depth_corrected'

anomaly_score <- function(object, newdata, aggregator = "median", ...){
  
  
  
  corrected_depths <- depth_corrected(object, newdata, ...)
  
  res <- compute_anomaly(apply(corrected_depths
                               
                               , 1
                               
                               , eval(as.symbol(aggregator))
                               
  )
  
  , nrow(newdata)
  
  )
  
  return(res)
  
}



#' @name depth_terminalNodes

#' @title depth_terminalNodes

#' @description depth_terminalNodes

#' @param treelike A single ranger tree extracted from 'ranger::treeInfo'

depth_terminalNodes <- function(treelike){
  
  
  
  nodeID     <- NULL
  
  leftChild  <- NULL
  
  rightChild <- NULL
  
  
  
  data.table::setDT(treelike)
  
  dropThese <- setdiff(colnames(treelike)
                       
                       , c("nodeID", "leftChild", "rightChild")
                       
  )
  
  treelike[, c(dropThese) := NULL]
  
  melted    <- data.table::melt(treelike
                                
                                , id.vars      = "nodeID"
                                
                                , measure.vars = c("leftChild", "rightChild")
                                
  )
  
  value       <- NULL
  
  edgeMat     <- as.matrix(melted[!is.na(value), c("nodeID", "value")]) + 1L
  
  treegraph   <- igraph::graph_from_edgelist(edgeMat)
  
  tnValues    <- treelike[is.na(leftChild) & is.na(rightChild), nodeID] + 1L
  
  depths      <- igraph::distances(treegraph
                                   
                                   , v    = 1
                                   
                                   , to   = tnValues
                                   
                                   , mode = "out"
                                   
  )
  
  dim(depths) <- NULL
  
  names(depths) <- tnValues
  
  return(depths)
  
}



#' @name average_path_length

#' @title average_path_length

#' @description average_path_length

#' @param n n

average_path_length <- Vectorize(
  
  function(n){
    
    ifelse(n == 1, 0, 2 * ( harmonic(n - 1) - ((n - 1)/n) ))
    
  }
  
  , vectorize.args = "n"
  
)



#' @name harmonic_approx

#' @title harmonic_approx

#' @description harmonic_approx

#' @param n n

harmonic_approx <- function(n){ log(n) + 0.577216 }



#' @name harmonic_exact

#' @title harmonic_exact

#' @description harmonic_exact

#' @param n n

harmonic_exact <-
  
  Vectorize(
    
    function(n){
      
      sum(1/seq(1,n))
      
    }
    
    , vectorize.args = "n"
    
  )



#' @name harmonic

#' @title harmonic

#' @description harmonic

#' @param n n

harmonic <- Vectorize(
  
  function(n){
    
    if(n < 11){
      
      harmonic_exact(n)
      
    } else {
      
      harmonic_approx(n)
      
    }
    
  }
  
  , vectorize.args = "n"
  
)



#' @name compute_anomaly

#' @title compute_anomaly

#' @description compute_anomaly

#' @param pathLength pathLength

#' @param dataSize dataSize

compute_anomaly <- Vectorize(
  
  function(pathLength, dataSize){
    
    2^( -( pathLength / average_path_length(dataSize) ) )
    
  }
  
  , vectorize.args = "pathLength"
  
)


college_clean=read.csv("insurance cluter2.csv",header = T)
college_clean$bodily_injuries<-as.factor(college_clean$bodily_injuries)
college_clean$witnesses<-as.factor(college_clean$witnesses)
iforest <- isolationForest(college_clean)

#predict outliers within dataset
college_clean$pred <- predict.solitude(iforest, college_clean, type = "anomaly_score")
college_clean$outlier <- as.factor(ifelse(college_clean$pred >=0.55, "outlier", "normal"))

r=data.frame(table(college_clean$outlier))

library(plotly)#plot data again with outliers identified
plot_ly(r,x = r$Var1,y=r$Freq)

