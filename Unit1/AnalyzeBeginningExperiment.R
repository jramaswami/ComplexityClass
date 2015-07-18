analyzeBegExpResults <- function(file_name) {
        df <- read.csv(file=file_name, skip=6, header=TRUE)
        dt <- data.table(df)
        mn <- dt[,mean(ticks), 
           by=.(population, max.step.size,max.turn.angle)
        ]
        mn
}

mn <- analyzeBegExpResults("BegExpResults.csv")
print("Population experiment")
print(mn[max.step.size==4 & max.turn.angle==60][order(population)])
print("Max.step.size experiment")
print(mn[population==50 & max.turn.angle==60][order(max.step.size)])
print("Max.turn.angle experiment")
print(mn[population==50 &max.step.size==4][order(max.turn.angle)])
