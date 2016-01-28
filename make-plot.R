library(readr)
library(dplyr)
library(ggplot2)
library(reshape2)
)

Migration<- read.csv("Migration.csv", header=TRUE, stringsAsFactors=FALSE)
PlaceCode<- read.csv("PlaceCode.csv", header=TRUE, stringsAsFactors=FALSE)
colnames(Migration)[2]<-"Value"  # to use join function with PlaceCode data
New_Migration <- left_join(Migration, PlaceCode, by = "Value")
names(New_Migration)[5]<-"BirthPlace"

# Count the number moving from each place each year
mig_summary <- New_Migration %>% group_by(BirthPlace, YEAR) %>% summarise(n = n())

# Top five places that makes the highest number of moves to Oregon
total <- mig_summary %>%    
  group_by(BirthPlace) %>% 
  summarise(total = sum(n))
top<- total[order(total$total, decreasing=TRUE),]
top_five<- top[1:5,]

# Grouping places other than "top five" into different regions 
others <- mig_summary %>% anti_join(top_five, by= "BirthPlace") 
others$BirthPlace <- with(others, 
                          ifelse(BirthPlace %in% c("California","Oregon","Washington","Arizona","Colorado","Idaho","Montana","New Mexico","Nevada","Utah","Wyoming"), "West", 
                                 ifelse(BirthPlace %in% c("Illinois","Iowa","Kansas","Missouri","Michigan","Minnesota","North Dakota","Nebraska","Ohio","Indiana","South Dakota","Wisconsin"), "MidWest", 
                                        ifelse(BirthPlace %in% c("Alabama","Arkansas","Delaware","Florida", "Georgia", "Kentucky","Louisiana", "Mississippi", "Maryland", "North Carolina",  "Oklahama", "South Carolina", "Texas", "Tennessee", "Virginia", "West Virginia"), "South",
                                               ifelse(BirthPlace %in% c("Connecticut","Maine","Massachusetts", "New Hampshire", "New Jersey","New York", "Pennsylvania"   , "Rhode Island", "Vermont"), "Northeast",    "Other" )))))

others<- others %>%
  group_by(YEAR, BirthPlace) %>%
  summarise(n = sum(n))

top_full<- top_five %>% left_join(mig_summary, by="BirthPlace")
top_full$total<- NULL  # to be able to row bind with others data
full<- rbind(top_full, others)

# Getting the proportion of each place number of moves per year
total_moves_year<- full %>% group_by (YEAR) %>% summarise(total= sum(n))
final<- left_join(full,total_moves_year, by="YEAR")
final$percent<- (final$n/final$total)*100

# Making the order of places similar to original plot
final$BirthPlace <- factor(final$BirthPlace, levels=c("Other","MidWest", "West","South","Northeast","Idaho","Mexico","Washington","California", "Oregon"))
final<- final[order(final$BirthPlace),] # order the BirthPlace variables according to the original plot

# Making the plot
theme_custom<- theme( title = element_text(size=14, face="bold"),
                      panel.border = element_blank(),
                      panel.grid= element_blank(),
                      axis.title.x = element_blank(),
                      axis.title.y = element_text(size=12),
                      panel.background = element_blank(),
                      legend.text = element_text(size = 10),
                      legend.title = element_text(size= 10),
                      axis.ticks = element_blank()
)

ggplot(data= final)+ geom_area(aes(x= YEAR, y= percent, fill= BirthPlace), colour="white", stat= "identity", position="stack")+
  scale_fill_manual( values=rev(c("darkgrey", "gold","gold","firebrick", "gold",
                                  "plum", "lawngreen", "gold","skyblue2","grey")), name="Birth Place") +
  guides(fill = guide_legend(override.aes = list(colour = NULL), reverse =TRUE ))+
  labs(title="Where people living in Oregon where born", y="Percent") +# xlab("test")+ ylab("test2")+
  theme_custom

