#code by Kylie Langlois
#kylinnea@gmail.com
#@KylieLanglois on twitter

#packages--------------
library(ggplot2)
library(reshape2)
library(plyr)
library(ggpubr)

set.seed(10) #for reproducibility 

#files------------
#abundance table--
dat<-read.delim(file.choose(),head=T) #for tsv, txt
#dat<-read.csv(file.choose(),head=T) #for csv
#PUT YOURFILENAME.TXT HERE to keep track
dat1<-dat  #keep a "clean copy"

#mapping file--
map<-read.delim(file.choose(),head=T)
#map<-read.csv(file.choose(),head=T) #for csv
#PUT YOURMAPPINGFILE.TXT HERE
map1<-map

#taxonomy table--
tax<-read.delim(file.choose(),head=T) #for tsv, txt
tax<-read.csv(file.choose(),head=T) #for csv
#PUT YOURFILENAME.TXT HERE to keep track
tax1<-tax  #keep a "clean copy"

#function inventory--
fun<-read.csv(file.choose(),head=T)
#2021_0118_16S_genera_function_inventory_refs.csv
#from https://github.com/kylielanglois/16S_genera_potential_function_inventory
fun1<-fun

#manipulate files----------
#make abundance table proportional--
head(dat1[1:10])
#   X.OTU.ID X2.1 X2.2.1 X2.2.2 X2.2.3 X2.2.4 X2.2.5 X4.1 X4.2.1 X4.2.2
#1     seq1    4     23      8     17   6416   9845   18     21     20
#2     seq2   27     36     26    390   4854   2444   29     37     23
#3     seq3    4      0      0    235   3337    796    0      0      0
#4     seq4 2365   4238   3648     68     17     12 5308    193    641
#5     seq5    9     17      9     31   1543   3619   13     18     16
#6     seq6 1388    861    694     78     24     32 1081    161   2838

seqsum<-colSums(dat1[-1]) #be sure to ignore your "seq/ASV/OTU" column
dat1[,-1]<-sweep(dat1[,-1], 2, seqsum, `/`)*100 #make proportional

#check taxonomy--
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

#function inventory--
fun1<-fun[,c(2,4,5)]
fun1$gen<-trimws(toupper(fun1$gen))

#plot genera-------
colnames(dat1)[1]<-"ASV" #match abundance table and taxonomy file
colnames(tax1)[2]<-"ASV"

dat1$ASV<-trimws(dat1$ASV)
tax1$ASV<-trimws(tax1$ASV)
taxdat<-merge(dat1, tax1, by="ASV", all.x=T)
#match taxonomy to abundance table

taxdat.consol<-ddply(taxdat, "last_tax", numcolwise(sum))
#consolidate by genus
#can pick and choose genus based on sequence abundance, 
#function (if you have it), or other groups
#do that here before melt
#id.vars will be what is on the Y AXIS

m2<-melt(taxdat.consol)
m2$number<-1:length(m2$last_tax) #add a unique column
m2.fun<-merge(m2, fun1, by.x="last_tax", by.y="gen", all.x = T, sort=F)
m2.fun<-m2.fun[!duplicated(m2.fun$number),] #sometimes "merge" causes duplicates, so need to delete them

m2.fun[m2.fun==0]<-NA #turn 0s to NAs for a cleaner look

#RECOMMEND: use "grepl" to only plot genera from desired functional groups 
m2.fun.1<-m2.fun[grepl("ammonia_oxidation", m2.fun$fun_consol),]
m2.fun.2<-m2.fun[grepl("nitrite_oxidation", m2.fun$fun_consol),]
m2.fun.comb<-rbind(m2.fun.1, m2.fun.2)

g2<-ggplot(m2.fun.comb)+
  geom_point(aes(x=variable, 
                 y=last_tax, 
                 fill=last_tax, 
                 size=value), pch=21, alpha=0.75)+  
  #pch=21 allows for a fill color with black outline
  #alpha=transparency
  scale_size_continuous(range=c(0,15), breaks=c(0, 5, 10, 15))+
  facet_wrap(~fun_consol, scales = "free_y", ncol=1, nrow=2)+
  #can adjust scale size to fit data
  scale_x_discrete(labels=NULL)+ #if you want to change your tick mark labels
  theme(legend.position = "right",
        axis.text.x = element_text(angle=90, 
                                   color = "black",  size = 8),
        axis.text.y = element_text(size=10))+
  #theme items are adjustable
  ggtitle("Your title") +
  labs(x="Sample", y="", size="Percent")+
  guides(fill=F)
g2
#will get a warning that rows have been removed for missing points (NAs)
ggsave(g2,
       filename = "file_name_bubble_1.pdf",
       path = "/you_local_path/",
       height=6, width=8)

#plot functional groups-------
colnames(dat1)[1]<-"ASV" #match abundance table and taxonomy file
colnames(tax1)[2]<-"ASV"

dat1$ASV<-trimws(dat1$ASV)
tax1$ASV<-trimws(tax1$ASV)
taxdat<-merge(dat1, tax1, by="ASV", all.x=T)
#match taxonomy to abundance table

taxdat.consol<-ddply(taxdat, "last_tax", numcolwise(sum))
#consolidate by genus
taxdat.consol$num<-1:length(taxdat.consol$last_tax)

taxdat.consol$last_tax<-trimws(taxdat.consol$last_tax)
fun1$gen<-trimws(fun1$gen)
taxdat.fun<-merge(taxdat.consol, fun1, by.x="last_tax", by.y="gen", all.x=T, sort = F)
taxdat.fun<-taxdat.fun[!duplicated(taxdat.fun$num),]
taxdat.fun<-taxdat.fun[,-31] #remove unique number column

taxdat.fun.consol<-ddply(taxdat.fun, "fun_consol", numcolwise(sum))
#consolidate by function
taxdat.fun.consol<-taxdat.fun.consol[-90,] #remove NA row

taxdat.fun.consol$count<-rowSums(taxdat.fun.consol[2:length(colnames(taxdat.fun.consol))] != 0) #get number that each function appears
taxdat.fun.consol<-subset(taxdat.fun.consol, taxdat.fun.consol$count>20) 
#filter rare functions (typically filter at 5-10 samples)
taxdat.fun.consol<-taxdat.fun.consol[,-31] #remove counts column

fun.m<-melt(taxdat.fun.consol)

fun.m[fun.m==0]<-NA

#plot -----
fung<-ggplot(fun.m)+
  geom_point(aes(x=variable, 
                 y=fun_consol, 
                 fill=fun_consol, 
                 size=value), pch=21, alpha=0.75)+ 
  scale_size_continuous(range=c(0,18), 
                        breaks=c(0.5, 1, 5, 10, 25, 40))+
  scale_x_discrete(labels=NULL)+
  theme(legend.spacing.y = unit(0.1, "lines"), 
        legend.key.size = unit(0.75, "lines"),
        legend.position = "right",
        axis.text.x = element_text(angle=90, 
                                   color = "black",  size = 6),
        axis.text.y = element_text(size=12),
        plot.title = element_text(hjust=0.5, vjust = 0.5))+
  ggtitle("Title") +
  labs(x="Sample", y="", size="Percent")+
  guides(fill=F)
fung
#will get a warning that rows have been removed for missing points (NAs)

ggsave(fung,
       filename = "file_name_bubble.pdf",
       path = "/your_local_path/",
       height=6, width=8)

