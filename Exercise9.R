CV <- function(dir, col, sep = ",", header = TRUE, stringsAsFactors = FALSE) {
  #create empty vector for results
  results <- numeric(length = length(list.files(dir)))
  #create vector of list of files
  list <- list.files(dir)
  #for loop to check if each file has 50+ observations
  for(file in list){
    #load files
    table <- read.table(file = paste(dir, file, sep = "/"), sep = sep, header = header, stringsAsFactors = stringsAsFactors)
    if(length(table[,col] < 50)){
      print("Error: one or more files contain less than 50 observations.")
      input <- readline("Continue anyway? y/n ")
      if(input == "y"){
        print("Warning: one or more files contain less than 50 observations.")
        break
      }else if(input == "n"){
        stop()
      }
    }
  }
  #for loop of all files in directory
  for(n in 1:length(list)){
    #read each file
    table <- read.table(file = paste(dir, list[n], sep = "/"), sep = sep, header = header, stringsAsFactors = stringsAsFactors)
    #calculate CV of specified column
    cv <- sd(table[,col]) / mean(table[,col])
    #add CV to results vector
    results[n] <- cv
  }
  return(results)
}

