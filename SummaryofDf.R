SummaryofDf=function(data){
  if(!is.data.frame(data))
  {
    return(warning("given input is not a data frame"))
  }
  #To get the number of columns
  cols =ncol(data)
  #generate the numbers from 1 to number of columns
  seqNumber =seq(1,cols)
  #create a default data frame
  result <- data.frame()
  #for loop to get the column names and missing values
  for(p in seqNumber)
  {
    #get the column name of the feature based on the indexing
    colName=colnames(data)[p]
    #Get the number of missing values
    NoofMissingValues=sum(is.na(data[,p]))
    #Calculate unique values
    NoofUniqueValues =length(unique(data[,p]))
    #Calculate the missing values in proportion
    MissingValueproportion= NoofMissingValues/length(data[,p])
    if(class(data[,p])=="integer"){
    Mean=mean(data[,p],na.rm = T)
    Median=median(data[,p],na.rm = T)
    Stdev=sd(data[,p],na.rm = T)
    }else{
      Mean=NA
      Median=NA
      Stdev=NA
    }
    datatype=class(data[,p])
    #use cbind to merge colname and missing values
    x= cbind(colName,NoofMissingValues,NoofUniqueValues,round(MissingValueproportion,5),Mean,Stdev,Median,datatype)
    #bind the rows to the data frame
    result= rbind(result,x)
  }
  #Assign column names to the data frame
  colnames(result) =c("Column Name","No. of Missing Values","No of Unique Values","Missing Value Proportion","Mean","Stdev","Median","Data Type")
  #return the result
  return(result)
}





