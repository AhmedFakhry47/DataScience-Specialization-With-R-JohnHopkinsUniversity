complete <- function(direct, id=1:332){
    file_names = list.files(direct,pattern="*.csv")
    for (i in id) {
        filedir = paste(direct,file_names[i],sep="/")
        df = read.csv(file=filedir, header=TRUE, sep=",")
        if (i==id[1]) {
            observations <- sum(complete.cases(df))
        } 
        else {
            observations = c(observations,sum(complete.cases(df)))
        }
    }
    dataf <- data.frame("id" = id, "nobs" = observations)
    dataf
}
