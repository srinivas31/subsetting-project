rankall <- function(outcome, num = "best") 
  
{
  out <- outcome
  value <- num
  
  if(value=="best")
    Final <- bestall(out)
  if(value=="worst")
    Final <- worstall(out)  
  
  outcome_care <- read.csv("outcome.csv",  colClasses="character", na.strings="NA")
  names(outcome_care)[11] = "heart.attack"
  names(outcome_care) [17] = "heart.failure"
  names(outcome_care)[23] = "pneumonia"
  DF <- outcome_care[, c(2,7,11,17,23)]
  ## --------------------------------------
  abv <- unique(DF$State)
  code<- sort(abv)
  state <- character()
  hospital <- character()
  a<- DF$State
 ##-------------------------------------------- 
  x <- c("heart attack", "heart failure", "pneumonia")
  if (!(outcome %in% x))
    stop("invalid outcome")
  ##-----------------------------------------------------
  
  if(outcome=="heart attack" & class(num)=="numeric")
    
  {
    for (i in seq_along(code))
    {
    y<-DF[a==code[i], c(1,3)]
    y$heart.attack <- as.numeric(y$heart.attack)
    sorted <-  y[order(y$heart.attack, y$Hospital.Name),] 
    hosp <- as.vector(sorted[num,1])
    st <- code[i]
    state<-append(state,st)
    hospital <- append(hospital,hosp)
    }
    Final <- data.frame(hospital, state)
  }
  
  
  
  if(outcome=="heart failure" & class(num)=="numeric")
  {
    for (i in seq_along(code))
    {
    y<-DF[a==code[i], c(1,4)] 
    y$heart.failure <- as.numeric(y$heart.failure)
    sorted <-  y[order(y$heart.failure, y$Hospital.Name),]
    hosp <- as.vector(sorted[num,1])
    st <- code[i]
    state<-append(state,st)
    hospital <- append(hospital,hosp)
    }
    Final <- data.frame(hospital, state)
  }
  
  
  
  if(outcome=="pneumonia" & class(num)=="numeric")
  {
    for (i in seq_along(code))
    {
    y<-DF[a==code[i], c(1,5)]  
    y$pneumonia <- as.numeric(y$pneumonia)
    sorted <-  y[order(y$pneumonia, y$Hospital.Name),]
    hosp <- as.vector(sorted[num,1])
    st <- code[i]
    state<-append(state,st)
    hospital <- append(hospital,hosp)
    }
    Final <- data.frame(hospital, state)
  }
  
  
  Final
  
}