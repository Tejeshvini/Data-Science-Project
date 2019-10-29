library(mice)
library(readr)

files=list.files()
test=""
for (file in files){
  if(grepl("final", file, fixed=TRUE)){
    continue
  }
  if(grepl("csv", file, fixed=TRUE))
  {
    headers = read.csv(file, skip = 3, header = F, nrows = 1, as.is = T)
    df = read.csv(file, skip = 5, header = F)
    # drop last column (2018 no data -> all nulls)
    df <- df[c(1:63)]
    # create combined data frame for output of all indicators in this file after imputation
    combined <- data.frame(matrix(nrow=0, ncol=63))
    names(combined) <- names(df)
    # determine unique indicators
    indicators=unique(df["V4"])
    # loop through each indicator to impute
    error_1=list()
    error_2=list()
    for (indicator in indicators[,"V4"]){
      # debug print incase an interation fail we know which indicator failed
      print(indicator)
      
      # subset of each indicator (what is iterated over this for loop)
      dfset = subset(df, V4==indicator)
      # further split subset into left and right, where left is the (country name, code, indicator name, code, etc.)
      # each row/observation is a country for this indicator subset
      dfset_left <- dfset[c(1:4)]
      # and right is the (years 1960-2017)
      dfset_right <- dfset[c(48:63)]
      # MICE imputation
      result = tryCatch({
        init = mice(dfset_right, maxit=5)
      },  error = function(e) {
        error_1=append(error_1,indicator)
        print("error")
        imputed<-dfset_right
        imputed <- cbind(dfset_left, imputed)
      }, finally = {
        meth = init$method
        predM = init$predictorMatrix
        set.seed(103)
        result = tryCatch({
          imputed = mice(dfset_right, method="pmm", predictorMatrix=predM, m=5)
          imputed <- complete(imputed)
          imputed <- cbind(dfset_left, imputed)
        }, error = function(e) {
          
          error_2=append(error_2,indicator)
          print(indicator)
          imputed <- cbind (dfset_left, dfset_right)
          
        }, finally = {
          # Merge our left hand side (the country name, code, indicator name, code, etc.), back to the imputed output
        })
      })
      
      combined <- rbind(combined, imputed)
      # and then add to our total all indicator of this file to a data frame 
    }#for loop to skim through indicators
  }#Run on just valid csv files if statement
  write.csv( combined,paste("final",file))
  write.csv( error_2,paste("error_2",file))
  write.csv( error_1,paste("error_1",file))
  
}#End for loop through files