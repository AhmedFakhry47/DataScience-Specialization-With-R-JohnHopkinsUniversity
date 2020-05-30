corr <- function(direct,threshold = 0){
    corrvec<- vector()
    file_names = list.files(direct,pattern="*.csv")
    comp = complete(direct)
    comp = comp['id'][comp['nobs']>threshold]
    for (i in comp) {
        if (i>0) {
            filedir = paste(direct,file_names[i],sep="/")
            df = read.csv(file=filedir, header=TRUE, sep=",")
            sul = df[['sulfate']][complete.cases(df[['sulfate']],df[['nitrate']])]
            nit = df[['nitrate']][complete.cases(df[['sulfate']],df[['nitrate']])]
            
            if (i==comp[1]) {
                corrvec = cor(sul,nit,use="complete.obs")
            }
            else{
                corrvec = c(corrvec,cor(sul,nit,use="complete.obs"))
            }
        }
        else{
            corrvec = c(corrvec,0)
        }
    }
    corrvec
}
