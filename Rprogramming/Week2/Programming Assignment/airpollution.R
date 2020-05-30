pollutantmean <- function(dir='specdata',pollutant,ids= 1:332){
		
	means = vector(mode="numeric",length=length(ids))	
	ids   = list.files(paste(getwd(),dir,sep='/'))[ids]
	for (i in seq_along(ids) ){
		means[i] = mean(read.csv(file=paste(dir,ids[i],sep='/'))[,pollutant],na.rm=TRUE)
	}
	mean(means,na.rm=TRUE)
}	


complete <- function(dir,ids=1:332){
	comp   = vector(mode="integer",length=length(ids))
	files  = list.files(paste(getwd(),dir,sep='/'))[ids]

	for (i in seq_along(files)){
		comp[i] = nrow(read.csv(file=paste(dir,files[i],sep='/')))
	}
	
	data.frame("id"= ids, "nobs" = comp)
}

corr <- function(dir,threshold){
	values = vector(mode="numeric")
	files  = list.files(paste(getwd(),dir,sep='/'))
	
	for (f in seq_along(files)){
		data <- read.csv(file=paste(dir,files[f],sep='/'))
		if(nrow(data) >= threshold){
			values <- c(values,cor(x=data$nitrate,y=data$sulfate,use="pairwise.complete.obs"))		
		}		 
	}	
	values
}



