#code by Kylie Langlois
#kylie.langlois@stonybrook.edu
#@KylieLanglois on twitter
#packages--------------
library(ggplot2)
library(plyr)
library(ggpubr)
library(egg)

set.seed(10) #for reproducibility 

coords<-read.delim(file.choose(),head=T) #for tsv, txt
#coordinates file
#coords<-read.csv(file.choose(),head=T) #for csv
#PUT YOURFILENAME.TXT HERE to keep track
coords1<-coords #keep a "clean" copy

tail(coords1[,1:10])
#         X        Axis1       Axis2      Axis3        Axis4
#100               OP.5.7 -0.017399206  0.07351090  0.1790390 -0.024090152
#101               OP.5.8 -0.003053954  0.05724936  0.1771546  0.003455746
#102               OP.5.9 -0.013390662  0.07493334  0.1903428 -0.012153650
#103                OP.5F -0.224479738 -0.41938276 -0.1107863  0.031736473
#104              Eigvals  2.373096869  2.02852317  1.2898320  0.666582469
#105 Proportion_explained  0.232418923  0.19867169  0.1263250  0.065284474
coords1<-coords[1:103,] #cut off last two rows

map<-read.delim(file.choose(),head=T) 
#YOURFILENAME.TXT
map1<-map

#plotting ordination------
coords.map<-merge(coords1[,1:10], map1, by.x="X", by.y="X.SampleID")
#merge to guarentee the mapping attributes are in the same order as samples

#need to make characters for plotting
coords.map$layer<-as.character(coords.map$layer)
coords.map$depth<-as.character(coords.map$depth)
coords.map$project<-as.character(coords.map$project)
coords.map$system<-as.character(coords.map$system)

colcol<-c("#E69F00", "#009E73","#ADFFFF",  "black", "grey", "purple", "white")
#your color palette, or just use default colors

p1<- #1v2
  ggplot()+  
  geom_point(aes(x=coords.map$Axis1, 
                 y=coords.map$Axis2, 
                 fill=coords.map$layer, 
                 shape=coords.map$project), 
             size=5, color="black")+ 
  #"black" gets the outline for pch 21, 22, 23, 24, 25
  scale_shape_manual(values=c(21, 22, 23), name="Some variable",
                     label=c("1", "2", "3"))+
  scale_fill_manual(values=colcol, name="Some other variable", 
                    label=c("A", "B","C",  "D", "E", "F", "G"))+
  #must have variables for scale_shape_manual and scale_fill_manual be characters
  geom_text(aes(x=coords.map$Axis1,
                coords.map$Axis2, 
                label=coords.map$shortname), 
            vjust=-0.5, 
            col=alpha("black", 0.75), size=3)+
  #label the points with some variable
  xlab(label = paste("Axis 1", round(coords$Axis1[105], 4)*100, "%"))+ 
  #label for axis 1, last row of original "coords" file
  ylab(label = paste("Axis 2", round(coords$Axis2[105],4)*100, "%"))+ 
  #label for axis 2, last row of original "coords" file
  theme(legend.text = element_text(size=18),
        axis.title.x  = element_text(size = 16),
        axis.title.y  = element_text(size = 16),
        plot.title = element_text(hjust=0.5))+ #makes title centered
  ggtitle("Ordination plot (your distance metric)")
p1





