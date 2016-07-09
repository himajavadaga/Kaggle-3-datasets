### MODYFIYING BENCHARK FILE ###

#splitting the data into test and train as required in the original benchmark file
benchmark_train <- benchmark[((substr(benchmark$date,1,4)=="2009") | (substr(benchmark$date,1,4)=="2010")),]
benchmark_test <- benchmark[((substr(benchmark$date,1,4)=="2011") | (substr(benchmark$date,1,4)=="2012")),]

#adding column ID to the benchmark file
benchmark_test$id <- seq( 1, length(benchmark_test$date), by= 1)

#rearranging the columns as per the given format
benchmark_test = benchmark_test[,c(9,1,2,3,4,5,6,7,8)]

write.csv(benchmark_test,"benchmark.csv",row.names = FALSE)
