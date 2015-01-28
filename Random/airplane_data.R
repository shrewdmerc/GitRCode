library(XML)

tof <- "http://aviation-safety.net/statistics/phase/stats.php?phase=TOF"
icl <- "http://aviation-safety.net/statistics/phase/stats.php?phase=ICL"
enr <- "http://aviation-safety.net/statistics/phase/stats.php?phase=ENR"
apr <- "http://aviation-safety.net/statistics/phase/stats.php?phase=APR"
ldg <- "http://aviation-safety.net/statistics/phase/stats.php?phase=LDG"

accident_labels <- c("tof", "icl", "enr", "apr", "ldg")
TOF <- as.data.frame(readHTMLTable(tof))
ICL <- as.data.frame(readHTMLTable(icl))
ENR <- as.data.frame(readHTMLTable(enr))
APR <- as.data.frame(readHTMLTable(apr))
LDG <- as.data.frame(readHTMLTable(ldg))
accident_stats <- list(TOF,ICL,ENR,APR,LDG)

for(i in 1:length(accident_stats)) {
  colnames(accident_stats[[i]]) <- c("Year", 
                                   paste(accident_labels[i],"accidents", sep = "."),
                                   paste(accident_labels[i],"casualties", sep = "."))
}
TOF <- accident_stats[[1]]
ICL <- accident_stats[[2]]
ENR <- accident_stats[[3]]
APR <- accident_stats[[4]]
LDG <- accident_stats[[5]]

accidents <- as.data.frame(Reduce(function(x, y) merge(x, y, all=TRUE), accident_stats), stringsAsFactors = F)
accidents<- as.data.frame(sapply(accidents, FUN = function(x) as.numeric(as.character(x))))

for(i in 1:nrow(accidents)){
  accidents[i,"total.accidents"] <- sum(accidents[i,c(2,4,6,8,10)])
  accidents[i,"total.casualties"] <- sum(accidents[i,c(3,5,7,9,11)])
}

plot(accidents$total.accidents ~ accidents$Year)
plot(accidents$total.casualties ~ accidents$Year)
plot(accidents$total.casualties ~ accidents$total.accidents)
                        

