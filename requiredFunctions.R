print("Loading all the required functions")

# To find out how many unique tickets were formed
howManyTickets <- function(){
  names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
  names.len <- length(names)
  if("finalTickets" %in% ls(globalenv()) & names.len == 0){
    return(length(getUniqueTickets(get("finalTickets",globalenv()))))
  } else {
    finalTickets <- get("finalTickets",globalenv())
    temp <- lapply(finalTickets,namedTickets,name=names)
    temp <- Filter(Negate(is.null),temp)
    return(length(getUniqueTickets(temp)))
  }
}

# To get all the unique tickets
getUniqueTickets <- function(x){
  names <- lapply(x,function(y) y$Name)
  return(unique(names))
}


# To write the tickets in the file.
getTickets <- function(x,filename){
  if(class(x)=="data.frame"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x$Name),collapse=",")
  } else if (class(x)=="character"){
    playerNames <- paste0(gsub("(\\w+\\s?\\.?\\s?\\w+\\s?\\.?\\s?\\w*\\s?\\w+\\.?)","\"\\1\"",x),collapse=",")
  }
  write(playerNames,file=filename,append=TRUE)
}


# Salary check function
aboveSalary <- function(x,salary){
  ifelse(sum(x$Salary) >= salary, return(x), return(NULL))
}

# To find the tickets with given names
namedTickets <- function(ticket,name){
  if(sum(name %in% ticket$Name)==length(name)){
    return(ticket)
  } else {
    return(NULL)
  }
}

# To remove the tickets with given names
removeNamedTickets <- function(ticket,name,all=FALSE){
  if(sum(name %in% ticket$Name)==length(name) & all){
    return(NULL)
  } else if (sum(name %in% ticket$Name) > 0 & !all){
    return(NULL)
  } else {
    return(ticket)
  }
}

# To remove tickets with user provided names
removeTickets <- function(){
  if("finalTickets" %in% ls(globalenv())){
    finalTickets <- get("finalTickets",globalenv())
    names <- scan(file="",what=character(),nmax=9,quiet=TRUE,sep="\n")
    finalTickets <<- lapply(finalTickets,removeNamedTickets,names,all=FALSE)
    finalTickets <<- Filter(Negate(is.null),finalTickets)
  }
}