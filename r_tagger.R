library(markovchain)
getwd()
setwd("C:/Users/apradha7/Downloads/NLP/hmm_tagger")

#Reading the training set
conn = file("entrain.txt",open="r")
res<-readLines(conn)
data<-data.frame(Word=factor(),POS=factor())

for(i in 1:length(res)){
  row<-strsplit(res[i],"/")
  data<-rbind(data,data.frame(Word=row[[1]][1],POS=row[[1]][2]))
  print(res[i])
}

#Creating transition Matrix
p <- matrix(nrow=length(unique(data$POS)),ncol=length(unique(data$POS)),0)
for(t in 1:(length(data$POS)-1)){
  p[data$POS[t],data$POS[t+1]]<- p[data$POS[t],data$POS[t+1]]+1
}
for (i in 1:4) p[i, ] <- p[i, ] / sum(p[i, ])

mc <- markovchainFit(data=data$POS,method = "laplace", laplacian = 0.01)
tm<-mc$estimate@transitionMatrix

#Creating Observation Matrix
o<-table(data[, c("POS", "Word")])
o<-as.data.frame.matrix(o)
for (i in 1:nrow(o)) o[i,]<-((o[i,]+1)/(sum(o[i,])+length(data$POS)))
om<-data.matrix(o)

#Impelementing Viterbi Algo
observations <- colnames(om)

viterbi<-function(seq,tm,om){
  states<-rownames(om)
  v<-makeViterbi(seq,tm,om)
  vt3<-c()
  mostprob<-apply(v, 1, function(x) which.max(x))
  prevobs<-seq[1]
  prevmostprobstate<-mostprob[1]
  prevmostprobstatename<-states[prevmostprobstate]
  startpos <- 1
  for(i in 2:length(seq)){
    obs <- seq[i]
    mostprobstate<-mostprob[i]
    mostprobstatename <- states[mostprobstate]
    if(mostprobstatename != prevmostprobstatename){
      vt3[i-1]<-prevmostprobstatename
      # print(paste("Positions",startpos,"-",(i-1),"Most probable state= ",prevmostprobstatename))
      if(startpos-(i-1)!=0){
        for (a in startpos:(i-1)){
        vt3[a]<-prevmostprobstatename
        }
      }
      startpos <- i
    }
    prevobs<-obs
    prevmostprobstatename<-mostprobstatename
  }
  vt3[i]<-prevmostprobstatename
  return(vt3)
  # print(paste("Positions1",startpos,"-",i,"Most probabale state",prevmostprobstatename))
  }

makeViterbi<-function(seq,tm,om){
  row_names<-rownames(om)
  N=length(row_names)
  v<-matrix(NA,nrow=length(seq),ncol=dim(tm)[1])
  v[1,]<-0
  v[1,1]<-1
  for (i in 2:length(seq)){
    for (l in 1:N){
#       print(row_names[l])
#       print(seq[i])
      # print(om[row_names[l],seq[i]])
      stateprob <- tryCatch({om[row_names[l],seq[i]]},error =function(e){stateprob=1})
      v[i,l]<-stateprob * max(v[(i-1),]*tm[,l])
    }
  }
  return(v)
}  

#Reading Test data
conn1 = file("entest.txt",open="r")
res1<-readLines(conn1)
test_data<-data.frame(Word=factor(),POS=factor())

for(i in 1:length(res)){
  if(i!=1 & res1[i]=="###/###"){
    break
  }
  row1<-strsplit(res1[i],"/")
  test_data<-rbind(test_data,data.frame(Word=row1[[1]][1],POS=row1[[1]][2]))
  print(res1[i])
}
test_data<-na.omit(test_data)

#Implementing Viterbi in Test Set
start<-1
seq<-c()
tags<-c()
tagged_vector<-c()
while(start!=length(test_data$Word)){
  seq<-c()
  for(j in start:length(test_data$Word)){
    if(test_data$Word[j]=="###"&j!=start){
      o_start<-start
      start<-j
      
      break
    }
    seq<-append(seq,as.character(test_data[j,1]))
  }
  tags <- viterbi(seq,tm,om)
  tagged_vector<-append(tagged_vector,tags)
  print(start)
}
tagged_vector<-append(tagged_vector,"###")
test_data$POS_tags<-tagged_vector

#Calculating Error Rate
unmatch<-test_data[test_data$POS!=test_data$POS_tags,]
unmatch_count=nrow(unmatch)

error_rate=(unmatch_count/nrow(test_data))*100
print(paste("Error rate = ",error_rate,"%"))
