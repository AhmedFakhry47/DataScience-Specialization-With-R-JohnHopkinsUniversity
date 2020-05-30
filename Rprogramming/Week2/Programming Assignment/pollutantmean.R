pollutantmean <- function(direct, pollutant,id=1:332){
    file_names = list.files(direct,pattern="*.csv")
    for (i in id) {
        filedir = paste(direct,file_names[i],sep="/")
        if (i==id[1]) {
            df = read.csv(file=filedir, header=TRUE, sep=",")
        } 
        else {
            df = rbind(df, read.csv(file=filedir, header=TRUE, sep=","))
        }
    }
    col = df[[pollutant]]
    means = mean(col, na.rm = TRUE)
    means
}
