data<-read.csv(file.choose(), header=T, sep=";", fill=TRUE)#the csv file for this is RAforR
attach(data)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(gghighlight)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#D55E00", "#0072B2", "#CC79A7") #Creating a colour-blind-friendly palette
greyscale<-c("#000000", "#C0C0C0", "#696969", "#A9A9A9", "#000000") #creating a greyscale palette (as this was not to be published in colour originally)
quart1<-filter(data, ini<=4)
quart2<-filter(data, ini<=8, ini>4)
quart3<-filter(data, ini<=12, ini>8)
quart4<-filter(data, ini<=16, ini>12)
datawo<-data%>%filter(!gen%in%"gacetilla")
attach(datawo)
data$yr <- as.factor(data$yr)
datawo$yr <- as.factor(datawo$yr)

#structure of this file: 
#1. Average length of translated texts over time
#2. Average starting page of translated texts over time
#3. Bubble chart representing the whole magazine
#4. Introducing the signature type/mention of translation as a variable and combining it with the above
#5. Others


#1. Average length of translated texts over time

#average length per volume
lengthmean.data<-data.frame(year=c(1,2,3,4,5), transllengthmean=sapply(1:5, function(z)data%>%filter(yr==z)%>%select(ext)%>%sapply(mean,na.rm=TRUE)) ) #making a dataframe with the average length for each year (not forgetting to remove missing values). Thanks to Matthieu Wilhelm for the recursive solution!
attach(lengthmean.data)

#c1a scatterplot of average length of translations per volume
ggplot(lengthmean.data, aes(year, transllengthmean))+
  geom_point(size=3)+
  scale_y_continuous(limits = c(0, 2), expand = c(0,0))+
  labs(title="Average length of translations in the Revista Azul, per volume",
       subtitle="(with gacetillas)",
       x="Volume",
       y="Average length (in pages)",
       tag="Fig. c1a")

#c1b barplot of average length of translations per volume 
transllengthmeanrd<-round(transllengthmean,digits=2)
ggplot(lengthmean.data, aes(year, transllengthmean))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(limits = c(0, 2), expand = c(0,0))+
  labs(title="Average length of translations in the Revista Azul increases with time",
       subtitle="(with gacetillas)",
       x="Volume number",
       y="Average length (in pages)",
       tag="Fig. c1b")+
  ggrepel::geom_label_repel(label=transllengthmeanrd)

#Those two plots express the same - it is a matter of preference ultimately, I tend to find that the barplot is more telling.

#now same plots without gacetillas

lengthmean.datawo<-data.frame(year=c(1,2,3,4,5), transllengthmeanwo=sapply(1:5, function(z)datawo%>%filter(yr==z)%>%select(ext)%>%sapply(mean,na.rm=TRUE)) )
attach(lengthmean.datawo)

#c1a-w/o scatterplot of average length of translations per volume, without gacetillas
ggplot(lengthmean.datawo, aes(year, transllengthmeanwo))+
  geom_point(size=3)+
  scale_y_continuous(limits = c(0, 2), expand = c(0,0))+
  labs(title="Average length of translations in the Revista Azul, per volume",
       subtitle="(without gacetillas)",
       x="Volume",
       y="Average length (in pages)",
       tag="Fig. c1a-w/o")

#c1b-w/o barplot of average length of translations per volume, without gacetillas
transllengthmeanword<-round(transllengthmeanwo,digits=2)
ggplot(lengthmean.datawo, aes(year, transllengthmeanwo))+
  geom_bar(stat="identity", width=0.5)+
  scale_y_continuous(limits = c(0, 2), expand = c(0,0))+
  labs(title="Average length of translations in the Revista Azul",
       subtitle="(without gacetillas)",
       x="Volume number",
       y="Average length (in pages)",
       tag="Fig. c1b_w/o")+
  ggrepel::geom_label_repel(label=transllengthmeanword)

#average length per issue
extissue.data <- data.frame(issue = 1:128, extmeanperissue=sapply(1:128, function(z)data%>%filter(num==z)%>%select(ext)%>%sapply(mean,na.rm=TRUE))  )
attach(extissue.data)
extmeanperissuerd<-round(extmeanperissue,digits=2)

#This is figure 1, scatterplot with average length per issue 
ggplot(extissue.data, aes(issue, extmeanperissue))+
  geom_point()+
  scale_y_continuous(limits = c(0, 4.5), expand = c(0,0))+
  labs(title="Average length of translations in each issue of the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Average length (in pages)",
       tag="Fig. 1")

#same without gacetillas
extissue.datawo <- data.frame(issue = 1:128, extmeanperissuewo=sapply(1:128, function(z)data%>%filter(num==z)%>%select(ext)%>%sapply(mean,na.rm=TRUE))  )
attach(extissue.datawo)
extmeanperissueword<-round(extmeanperissuewo,digits=2)

#c1-w/o scatterplot with average length per issue, without gacetillas
ggplot(extissue.datawo, aes(issue, extmeanperissuewo))+
  geom_point()+
  scale_y_continuous(limits = c(0, 4.5),expand = c(0,0))+
  labs(title="Average length of translations in each issue of the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Average length (in pages)",
       tag="Fig. c1-w/o")


#Using the sum to track the evolution of length
#c1c sum of length of translations in each issue
sumoflenghtnum<-aggregate(data$ext, by=list(data$num), FUN=sum)
sumoflenghtnum[is.na(sumoflenghtnum)] <- 0 #so that the issues with no translation do not appear as N/A but rather as 0 pages of translation in total. 

ggplot(sumoflenghtnum, aes(Group.1, x))+
  scale_y_continuous(limits=c(0,10), expand = c(0,0))+
  geom_bar(stat="identity")+
  labs(title="Space occupied by translations in each issue of the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1c")

#c1d sum of length of translations in each volume / No idea why this is not working
sumoflenghtyr<-aggregate(data$ext, by=list(data$yr), FUN=sum) #this is the problematic line
ggplot(sumoflenghtyr, aes(Group.1, x))+
  scale_y_continuous(expand = c(0,0))+
  geom_bar(stat="identity", width=0.5)+
  labs(title="Space occupied by translations in each volume of the Revista Azul",
       subtitle="(with gacetillas)",
       x="Volume number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1d")

#c1c-w/o same without gacetillas
sumoflenghtnumwo<-aggregate(datawo$ext, by=list(datawo$num), FUN=sum)
sumoflenghtnumwo[is.na(sumoflenghtnumwo)] <- 0 

ggplot(sumoflenghtnumwo, aes(Group.1, x))+
  geom_bar(stat="identity")+
  labs(title="Space occupied by translations in each issue of the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1c-w/o")

#c1d-w/o
sumoflenghtyrwo<-aggregate(datawo$ext, by=list(datawo$yr), FUN=sum) #this is the problematic line
ggplot(sumoflenghtyrwo, aes(Group.1, x))+
  scale_y_continuous(expand = c(0,0))+
  geom_bar(stat="identity", width=0.5)+
  labs(title="Space occupied by translations in each volume of the Revista Azul",
       subtitle="(without gacetillas)",
       x="Volume number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1d-w/o")


#sum expressed as scatterplot
#c1e scatterplot of sum of lengths per issue
ggplot(sumoflenghtnum, aes(Group.1,x))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+
  scale_y_continuous(limits = c(0, 10))+
  labs(title="Space occupied by translations in each issue of the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1e")

#c1e-w/o
ggplot(sumoflenghtnumwo, aes(Group.1,x))+
  geom_point()+
  geom_smooth(method='lm',formula=y~x)+
  scale_y_continuous(limits = c(0, 10))+
  labs(title="Space occupied by translations in each issue of the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Sum of lengths (in number of pages)",
       tag="Fig. c1e-w/o")

#COMMENT ON SUM TO COME


#2. Average starting page of translated texts over time

#average starting page per volume, with gacetillas
posmean.data<-data.frame(year=c(1,2,3,4,5), translpositionmean=sapply(1:5, function(z)data%>%filter(yr==z)%>%select(ini)%>%sapply(mean,na.rm=TRUE)) ) 
attach(posmean.data)
translpositionmeanrd<-round(translpositionmean,digits=2)

#c2a barplot with average initial page of translations per volume
ggplot(posmean.data, aes(year, translpositionmean))+
  geom_bar(stat="identity", width = 0.5)+
  scale_y_continuous(limits=c(0,10), expand = c(0,0))+
  ggrepel::geom_label_repel(label=translpositionmeanrd)+
  labs(title="Average position of translations in the Revista Azul",
       subtitle="(with gacetillas)",
       x="Volume number",
       y="Average starting page (page number)",
       tag="Fig. c2a")

#same without gacetillas
posmean.datawo<-data.frame(year=c(1,2,3,4,5), translpositionmeanwo=sapply(1:5, function(z)datawo%>%filter(yr==z)%>%select(ini)%>%sapply(mean,na.rm=TRUE)) ) 
attach(posmean.datawo)
translpositionmeanword<-round(translpositionmeanwo,digits=2)

#c2a-w/o barplot with average initial page of translations per volume
ggplot(posmean.datawo, aes(year, translpositionmeanwo))+
  geom_bar(stat="identity", width = 0.5)+
  scale_y_continuous(limits=c(0,10), expand = c(0,0))+
  ggrepel::geom_label_repel(label=translpositionmeanword)+
  labs(title="Average position of translations in the Revista Azul",
       subtitle="(without gacetillas)",
       x="Volume number",
       y="Average starting page (page number)",
       tag="Fig. c2a-w/o")


#Average position in each issue

posissue.data <- data.frame(issue = 1:128, positionmeanperissue=sapply(1:128, function(z)data%>%filter(num==z)%>%select(ini)%>%sapply(mean,na.rm=TRUE))  )
attach(posissue.data)
positionmeanperissuerd<-round(positionmeanperissue,digits=2)


#This is figure 2, scatterplot of the average position per issue
ggplot(posissue.data, aes(issue, positionmeanperissue))+
  geom_point()+
  scale_y_continuous(limits = c(0, 16), expand = c(0,0)) +
  labs(title="Average position of translations in each issue of the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Average starting page (page number)",
       tag="Fig. 2")

#same without gacetillas
posissue.datawo <- data.frame(issue = 1:128, positionmeanperissuewo=sapply(1:128, function(z)datawo%>%filter(num==z)%>%select(ini)%>%sapply(mean,na.rm=TRUE))  )
attach(posissue.datawo)
positionmeanperissueword<-round(positionmeanperissuewo,digits=2)

#c2-w/o scatterplot of average starting page of translations per issue of the Revista Azul, without gacetillas
ggplot(posissue.datawo, aes(issue, positionmeanperissuewo))+
  geom_point()+
  scale_y_continuous(limits = c(0, 16), expand = c(0,0)) +
  labs(title="Average position of translations in each issue of the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Average starting page (page number)",
       tag="Fig. c2-w/o")

#translations published on pages 1-4, per year
ini25yr1<-data%>%filter(yr==1)%>%filter(ini<=4)
ini25yr2<-data%>%filter(yr==2)%>%filter(ini<=4)
ini25yr3<-data%>%filter(yr==3)%>%filter(ini<=4)
ini25yr4<-data%>%filter(yr==4)%>%filter(ini<=4)
ini25yr5<-data%>%filter(yr==5)%>%filter(ini<=4)

#translations published on pages 9-12, per year 
ini50yr1<-data%>%filter(yr==1)%>%filter(ini>=9, ini<=12)
ini50yr2<-data%>%filter(yr==2)%>%filter(ini>=9, ini<=12)
ini50yr3<-data%>%filter(yr==3)%>%filter(ini>=9, ini<=12)
ini50yr4<-data%>%filter(yr==4)%>%filter(ini>=9, ini<=12)
ini50yr5<-data%>%filter(yr==5)%>%filter(ini>=9, ini<=12)

#average length of translations published on pages 1-4, per year
ini25yr1averageext<-data%>%filter(yr==1)%>%filter(ini<=4)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini25yr2averageext<-data%>%filter(yr==2)%>%filter(ini<=4)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini25yr3averageext<-data%>%filter(yr==3)%>%filter(ini<=4)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini25yr4averageext<-data%>%filter(yr==4)%>%filter(ini<=4)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini25yr5averageext<-data%>%filter(yr==5)%>%filter(ini<=4)%>%select(ext)%>%sapply(mean,na.rm=TRUE)

#average length of translations published on pages 9-12, per year
ini50yr1averageext<-data%>%filter(yr==1)%>%filter(ini>=9, ini<=12)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini50yr2averageext<-data%>%filter(yr==2)%>%filter(ini>=9, ini<=12)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini50yr3averageext<-data%>%filter(yr==3)%>%filter(ini>=9, ini<=12)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini50yr4averageext<-data%>%filter(yr==4)%>%filter(ini>=9, ini<=12)%>%select(ext)%>%sapply(mean,na.rm=TRUE)
ini50yr5averageext<-data%>%filter(yr==5)%>%filter(ini>=9, ini<=12)%>%select(ext)%>%sapply(mean,na.rm=TRUE)


#3. The master of all charts: bubble chart representing the space of the magazine
#the size of the points represents the length of the translations, while they are 

#c3a bubble chart in colour-blind-proof tones, coloured by volume
ggplot(data, aes(num, ini))+
  geom_point(size=3*data$ext, alpha = 0.6, aes(colour=yr))+
  scale_colour_manual(values=cbbPalette)+
  scale_y_continuous(limits = c(0, 17), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Starting page",
       color="Volume",
       tag="Fig. c3a")

#c3a-w/o same without gacetillas
ggplot(datawo, aes(num, ini))+
  geom_point(size=3*datawo$ext, alpha = 0.6, aes(colour=yr))+
  scale_colour_manual(values=cbbPalette)+
  scale_y_continuous(limits = c(0, 17), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Starting page",
       color="Volume",
       tag="Fig. c3a-w/o")

#for the bubble chart, the presence or absence of gacetillas does not make a world of difference...

#This is figure 3. bubble chart in grey scale, coloured by volume
ggplot(data, aes(num, ini))+
  geom_point(size=3*data$ext, alpha = 0.7, aes(colour=yr))+
  scale_color_manual(values=greyscale)+
  scale_y_continuous(limits = c(0, 17), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Starting page (page number)",
       color="Volume",
       caption="The size of the bubbles represents the length of the translations",
       tag="Fig. 3")

#c3-w/o same without gacetillas
ggplot(datawo, aes(num, ini))+
  geom_point(size=3*datawo$ext, alpha = 0.7, aes(colour=yr))+
  scale_color_manual(values=greyscale)+
  scale_y_continuous(limits = c(0, 17), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Starting page (page number)",
       color="Volume",
       caption="The size of the bubbles represents the length of the translations",
       tag="Fig. c3-w/o")

#c3b different colour coding/by position (still with the colourblind-friendly scheme)
ggplot(data, aes(num, ini))+
  geom_point(data=quart1, aes(size=ext), alpha=0.6, colour="#E69F00", show.legend = FALSE)+
  geom_point(data=quart2, aes(size=ext), alpha=0.6, colour="#56B4E9", show.legend = FALSE)+
  geom_point(data=quart3, aes(size=ext), alpha=0.6, colour="#D55E00", show.legend = FALSE)+
  geom_point(data=quart4, aes(size=ext), alpha=0.6, colour="#0072B2", show.legend = FALSE)+
  scale_size(range = c(1,18))+
  scale_y_continuous(limits = c(0, 17), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Starting page",
       tag="Fig. c3b")

#this colour coding is much less eloquent than the one based on the volume, however, it allowed me to distinguish a phenomenon that is more visible in the filtered version of this plot (see below)
#c3b-w/o is omitted as it does not bring anything new

#This is figure 3c, bubble chart containing only first and third quarters
ggplot(data, aes(num, ini))+
  geom_point(data=quart1, aes(size=ext), alpha=0.6, colour="#000000", show.legend = FALSE)+
  geom_point(data=quart3, aes(size=ext), alpha=0.6, colour="#000000", show.legend = FALSE)+
  scale_size(range = c(1,20))+
  scale_y_continuous(limits = c(0, 13), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul, pages 1-4 and 8-12 only",
       subtitle="(with gacetillas)",
       x="Issue number",
       y="Starting page (page number)",
       tag="Fig. 3c",
       caption="The size of the bubbles represents the length of the translations")

#c3c-w/o length and position of translations in first and last quarters, without gacetillas
ggplot(data, aes(num, ini))+
  geom_point(data=quart1%>%filter(!gen%in%"gacetilla"), aes(size=ext), alpha=0.6, colour="#000000", show.legend = FALSE)+
  geom_point(data=quart3%>%filter(!gen%in%"gacetilla"), aes(size=ext), alpha=0.6, colour="#000000", show.legend = FALSE)+
  scale_size(range = c(1,20))+
  scale_y_continuous(limits = c(0, 13), expand = c(0,0))+
  theme_classic()+
  labs(title="Length and position of translations in the Revista Azul, pages 1-4 and 8-12 only",
       subtitle="(without gacetillas)",
       x="Issue number",
       y="Starting page (page number)",
       tag="Fig. c3c-w/o",
       caption="The size of the bubbles represents the length of the translations")


#4. Introducing the signature/mention as a variable
#COMING SOON