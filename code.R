df=read.csv('C:/Users/ASUSPRO/Desktop/Data Science/Section 5/googleplaystore3-1.csv',header = T,stringsAsFactors = F)
head(df)
summary(df)
structure(df)
df$Category=factor(df$Category)
levels(df$Category)=seq(1:33)

unique(df$Type)
df$Type[df$Type==" "]=NA
df$Type=factor(df$Type)
levels(df$Type)
summary(df)
is.character(df$Last.Updated)
df$Last.Updated=as.Date(df$Last.Updated,tryFormats = c("%m/%d/%Y"))
df$Updates=Sys.Date()-df$Last.Updated
df$Updates=as.numeric(df$Updates)
str(df)



spinst=function(x){splinter=0
  spliter=strsplit(x,"")
  spliter=unlist(spliter)
  spliter=spliter[spliter!=","]
  spliter=spliter[spliter!="+"]
  spliter=paste(spliter,collapse = "")
  ;return(spliter)}
df$Installs=as.matrix(df$Installs)
df$Installs=apply(df$Installs,1,spinst)
df$Installs=as.numeric(df$Installs)
head(df)
str(df)

library(mice)
df=df[-c(1,2,7,8)]
head(df)
imput<-mice(df,m=2,meth=c("polyreg","sample","pmm","logreg","pmm"))
library(VIM)
aggr<- aggr(df, col=c('black','red'), numbers=TRUE, sortVars=TRUE, labels=names(df), cex.axis=.7, gap=1, ylab=c("Barplot of missing data","Patterns"))
head(df)
marginplot(df[,c(2,3)])
marginplot(df[,c(2,4)])
marginplot(df[,c(3,4)])
stripplot(imput, pch = 20, cex = 1.2)
densityplot(imput)
par(mfrow=c(2,2)) 
xyplot(imput,Rating ~ Type+Reviews|.imp,pch = 20, cex = 1.4)
xyplot(imput,Type ~ Rating+Reviews|.imp,pch = 20, cex = 1.4)
xyplot(imput,Reviews ~ Rating+Type|.imp,pch = 20, cex = 1.4)
df=complete(imput,1)
summary(df)
