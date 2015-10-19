library(markovchain)
library(HMM)
getwd()
setwd("C:/Users/apradha7/Downloads/NLP/hmm_tagger")


conn = file("entrain.txt",open="r")
res<-readLines(conn)
for(i in 1:length(res)){
  row<-strsplit(res[i],"/")
  data<-rbind(data,data.frame(Word=row[[1]][1],POS=row[[1]][2]))
  # data$Word[i]<-row[[1]][1]
  # data$POS[i]<-row[[1]][1]
  print(res[i])
}


# p <- matrix(nrow=length(unique(data$POS)),ncol=length(unique(data$POS)),0)
# 
# for(t in 1:(length(data$POS)-1)) p[data$POS[t],data$POS[t+1]]<- p[data$POS[t],data$POS[t+1]]+1
# 
# for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
# for (i in 1:4) p[i, ] <- p[i, ] / sum(p[i, ])
# 
# x <- c("A","B","A","A","C","D","D","A","B","D","A","D","C","D","D","D","C","A","C","B","C","C","C","D","B","B","C")
# x<-as.factor(x)
# p <- matrix(nrow = length(unique(x)), ncol = length(unique(x)), 0,dimnames=list(c(x.levels),c(x.levels)))
# for (t in 1:(length(x) - 1)) p[x[t], x[t + 1]] <- p[x[t], x[t + 1]] + 1
# for (i in 1:4) p[i, ] <- p[i, ] / sum(p[i, ])

mc <- markovchainFit(data=data$POS)
md <- as.data.frame(mc$estimate@transitionMatrix)
write.csv(md, file = "transition_matrix.csv")

