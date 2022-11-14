mult.wilcox=function(data,Sobs,Group,two.sided,BH)
{
  library(stats)
  
  colnames(data)
  colnamesdata<-as.data.frame(colnames(data))
  colnamesdata
  I=which(colnamesdata==deparse(substitute(Group)))
  data[,I]
  qwe=data[,I]
  qwe1=data.frame(id=c(1:length(qwe)),qwe=qwe)
  Bsobs=which(colnamesdata==deparse(substitute(Sobs)))
  ssobs=data[,Bsobs]
  qwe2=data.frame(id=c(1:length(ssobs)),ssobs=ssobs)
  merge1=merge(qwe1,qwe2,by="id")
  merge1<-merge1[,-1]
  merge1
  number=length(levels(qwe))
  number=(number*(number-1))/2
  number
  colnames(merge1)<-c("x","y")
  kong.matrix=matrix(1,length(unique(merge1[,"x"])),length(unique(merge1[,"x"])))
  type<-unique(merge1[,"x"])
  type
  merge1$class=as.numeric(merge1[,"x"])
  merge1
  class1=cbind(type,1:length(unique(merge1[,"x"])))
  class2=matrix(NA,length(merge1$class),1)
  for (i in 1:length(merge1$class))
  {
    class2[i,]=class1[which(merge1$class[i]==class1[,1]),2]
  }
  merge1=cbind(merge1,class2)
  merge1
  for(i in 1:length(unique(merge1$class2)))
  {
    data1<-subset(merge1,class2==i)
    data1
    
    data2<-merge1[-which(merge1$class2==i),]
    data2
    for(j in  c(unique(data2$class2)) )
    {
      data3<-subset(data2,class2==j) 
      sun=rbind(data1,data3)
      wilcox=wilcox.test(y ~ x, alternative = c(deparse(substitute(two.sided))),data = sun,exact = FALSE)
      p=wilcox$p.value
      p=p.adjust(p,method =deparse(substitute(BH)),number)
      kong.matrix[i,j]=p
      
    }
    sun
    kong.matrix
    rownames(kong.matrix)<-unique(merge1$x)
    colnames(kong.matrix)<-unique(merge1$x)
  }
  kong.matrix
  for(i in 1:(length(levels(qwe))-1))
  {
    kong.matrix[i+1,1:i]=NA
    kong.matrix
  }
  out=list(p.value=kong.matrix)
  return(out)
}
