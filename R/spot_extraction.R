spot_extraction <- function(file, start, end){
  
  # check that both spots_start and spots_end are present
  if(!is.na(start) & !is.na(end)){
    
    # create index for lines containing spots
    # spots themselves start two lines after start and one line before end
    index <- seq(start + 2, end - 1, 1)
  
    # extract list of lines of interest
    # apply strsplit function to separate values
    # simplify output and convert to data frame
    output <- as.list(file[index])
    output <- as.data.frame(sapply(output, function(x) strsplit(x, split = "\t")))
    
    # change column names
    # transpose
    # add correct names (first line after spots_start)
    # create column to identify each spot
    colnames(output) <- 1:ncol(output)
    output <- as.data.frame(t(output))
    colnames(output) <- unlist(strsplit(file[start + 1], split = "\t"))
    output <- dplyr::mutate(output,
                            spot = as.factor(1:nrow(output)), .before = 1)
    
    # convert each column to appropriate type
    # all stored as character previously
    output <- type.convert(output, as.is = TRUE)

  } else {
    
    # if start or end are missing, create empty data frame with no rows or columns
    output <- data.frame()[0,]
  }
  
  # return output data frame
  output
}
