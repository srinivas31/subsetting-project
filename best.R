
best <- function (state, outcome)

{
  
  
  outcome_care <- read.csv("outcome.csv",  colClasses="character")
  names(outcome_care)[11] = "heart.attack"
  names(outcome_care) [17] = "heart.failure"
  names(outcome_care)[23] = "pneumonia"
  

  
  
  a<-outcome_care$State
 
x <- c("heart attack", "heart failure", "pneumonia")
if (!(outcome %in% x))
  stop("invalid outcome")
  
if(length(state)!=0 & state %in%  a )
  
   {
  
        if(outcome=="heart attack")
          {
          
          y<-outcome_care[a==state, c(2,11)] 
           sorted <-  y[order(y$heart.attack, y$Hospital.Name),]
           name <- as.vector(sorted[1,1])
           
          }
        
        if(outcome=="heart failure")
        {
          
          y<-outcome_care[a==state, c(2,17)]
          y$heart.failure <- as.numeric(y$heart.failure)
          sorted <-  y[order(y$heart.failure, y$Hospital.Name),]
          name <- as.vector(sorted[1,1])
          
        }
        
        if(outcome=="pneumonia")
        {
          y<-outcome_care[a==state, c(2,23)] 
          y$pneumonia <- as.numeric(y$pneumonia)
          sorted <-  y[order(y$pneumonia, y$Hospital.Name),]
          name <- as.vector(sorted[1,1])
          
        }
  
  }
   
   else
   {
     stop ("invalid state") 
   }

  name
}
