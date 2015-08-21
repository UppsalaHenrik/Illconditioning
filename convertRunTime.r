
convertRunTime <- function(runTimes){
  

convRunTimes <- sapply(as.character(runTimes), FUN=function(x) {
  x <- unlist(strsplit(x, ":"))
  x <- as.numeric(x[1])*3600+as.numeric(x[2])*60+as.numeric(x[3])
})
names(convRunTimes) <- NULL



x <- runtimes[[2]]

y <- strsplit(runtimes[[2]], ":")

for(i in 1:length(x)){
  y[i] <- as.numeric(y[[i]][[1]])*3600+as.numeric(y[[i]][[2]])*60+as.numeric(y[[i]][[3]])
}


x <- runtimes[[2]]

x <- strsplit(x, ":")
x <- as.numeric(x[[1]][[1]])*3600+as.numeric(x[[1]][[2]])*60+as.numeric(x[[1]][[3]])
x <- x[[1]]

}




#[17:05:12] Aimee Gott Work: 
  
times <- list("2:30:05", "3:50:02")

runtimes2 <- sapply(times, FUN=function(x) {
  x <- unlist(strsplit(x, ":"))
  x <- as.numeric(x[1])*3600+as.numeric(x[2])*60+as.numeric(x[3])
  x
})
