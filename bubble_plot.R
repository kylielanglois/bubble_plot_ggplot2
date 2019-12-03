#code by Kylie Langlois
#kylie.langlois@stonybrook.edu
#@KylieLanglois on twitter
#packages--------------
library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)
library(egg)

set.seed(10) #for reproducibility 

#abundance table--------
dat<-read.delim(file.choose(),head=T) #for tsv, txt
#dat<-read.csv(file.choose(),head=T) #for csv
#PUT YOURFILENAME.TXT HERE to keep track
dat1<-dat  #keep a "clean copy"

head(dat1[1:10])
#       ASV X1X1.1 X1X2.4 X1X3.1 X2W1.1 X2W2.1 X3X1.2 X3X2.2 X3X3.2 X4W.1
#1     seq1     38     25     41     25     17    294     98    876  1771
#2     seq2      0      0      0      0      0      0      0      0     0
#3     seq3      0      0      0      0      0      0      0      0    10
#4     seq4      0     12      0    210    110      6      0      0  3353
#5     seq5      0      0      0      0      0      0      0      0     0
#6     seq6    183      0   4280      0      0      0      0    153     0

seqsum<-colSums(dat1) #be sure to ignore your "seq/ASV/OTU" column
dat1<-sweep(dat1, 2, seqsum, `/`)*100 #make proportional

#mapping file------
map<-read.delim(file.choose(),head=T)
#map<-read.csv(file.choose(),head=T) #for csv
#PUT YOURMAPPINGFILE.TXT HERE
map1<-map

#taxonomy table-----
tax<-read.delim(file.choose(),head=T) #for tsv, txt
#tax<-read.csv(file.choose(),head=T) #for csv
#PUT YOURFILENAME.TXT HERE to keep track
tax1<-tax  #keep a "clean copy"

head(tax1[1:9,])
#           genus       ASV
#2      ACIDOVORAX      seq30
#3   ACINETOBACTER     seq174
#4   ACINETOBACTER      seq56
#5   ACINETOBACTER     seq524
#6   ACINETOBACTER     seq248
#7   ACINETOBACTER     seq511
#8   ACINETOBACTER     seq129
#9   ACINETOBACTER     seq533
#10 ADURB.BIN063-1     seq640

#however you have it, you need your ASV = taxonomy

#plot ASV-------
#CANNOT DO ENTIRE ABUNDANCE TABLE, GGPLOT WILL CRASH
m1<-melt(dat1[1:10,])

g1<-ggplot()+
 geom_point(aes(x=m1$variable, 
                 y=m1$ASV, 
                 fill=m1$ASV, 
                 size=m1$value), pch=21, alpha=0.75)+  
  #pch=21 allows for a fill color with black outline
  #alpha=transparency
  scale_size_continuous(range=c(0,15), breaks=c(0, 5, 10, 15))+
  #can adjust scale size to fit data
  scale_x_discrete()+ #if you want to change your tick mark labels
  theme(legend.position = "top",
        axis.text.x = element_text(angle=90, 
                                   color = "black",  size = 8),
        axis.text.y = element_text(size=10))+
  #theme items are adjustable
  ggtitle("Your title") +
  labs(x="Sample", y="", size="Percent")+
  guides(fill=F)
g1

#plot genera-------
taxdat<-merge(dat1, tax1, by.x="ASV", by.y="ASV", all.x=T)
#match taxonomy to abundance table

taxdat.consol<-ddply(taxdat, "genus", numcolwise(sum))
#consolidate by genus
#can pick and choose genus based on sequence abundance, 
#function (if you have it), or other groups
#do that here before melt
#id.vars will be what is on the Y AXIS

m2<-melt(taxdat)

g2<-ggplot()+
  geom_point(aes(x=m2$variable, 
                 y=m2$last_tax, 
                 fill=m2$last_tax, 
                 size=m2$value), pch=21, alpha=0.75)+  
  #pch=21 allows for a fill color with black outline
  #alpha=transparency
  scale_size_continuous(range=c(0,15), breaks=c(0, 5, 10, 15))+
  #can adjust scale size to fit data
  scale_x_discrete()+ #if you want to change your tick mark labels
  theme(legend.position = "top",
        axis.text.x = element_text(angle=90, 
                                   color = "black",  size = 8),
        axis.text.y = element_text(size=10))+
  #theme items are adjustable
  ggtitle("Your title") +
  labs(x="Sample", y="", size="Percent")+
  guides(fill=F)
g2

