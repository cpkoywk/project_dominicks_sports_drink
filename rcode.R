library(xlsx)
demosmall <- read.xlsx("DEMOSMALL(3).xlsx", sheetName=1)
View(demosmall)
drinkhighmove <- read.csv("sports drinks high movement.csv")
upc<-read.xlsx("sports drinks upc.xlsx", sheetName=1)

#merge dataset
total1 <- merge(drinkhighmove,demosmall,by="STORE",all.x=TRUE)
total2<-merge(total1,upc,by="UPC",all.x=TRUE)
View(total1)
colnames(upc)

colnames(total2)
total2$BRAND.y<-NULL
total2$SIZE.y<-NULL
colnames(total2)[5]<-"BRAND"
colnames(total2)[6]<-"SIZE"

write.csv(total2,"FinalDF.csv",row.names = FALSE)
#read the dataset
df<-read.csv("FinalDF.csv")
#get rid of the exponent display
options("scipen" = 10)
head(df)
#How does the demand for a brand depend on price? What is the price elasiticity of demand of a brand.

#Select brand
#1200000735 ALL SPORT LEMON LIME 32 OZ 12 how many rows?
nrow(df[df$UPC=="1200000735",])
#4900002503 POWERADE LEMON-LIME 32 OZ 12
nrow(df[df$UPC=="4900002503",])
#5200003925 GATORADE LEMON/LIME 32 OZ 12
nrow(df[df$UPC=="5200003925",])


#find best selling stuff
bestsellingupc<-aggregate(df$MOVE, by=list(df$UPC), FUN=sum)
bestsellingupc <- bestsellingupc[order(bestsellingupc$x,decreasing = TRUE),] 

#best selling upcs
barplot(bestsellingupc$x,names.arg = bestsellingupc$Group.1,las=2,cex.names = 0.5,main = "UPCs with Most Moves")
text(cex=1, bestsellingupc$Group.1, xpd=TRUE, srt=45)



#these are the best seller from the three brands (with most moves), respectively

  #1. 5200003925 GATORADE LEMON/LIME 32 OZ 12
  #2. 1200000757 ALL SPORT FRUIT PUNC 32 OZ 12
  #3. 4900001923 POWERADE FRUIT PUNCH 32 OZ 12

#subset the dataset to only include these 3 UPCs
newdf<-df[df$UPC=="5200003925"|df$UPC=="1200000757"|df$UPC=="4900001923",]
write.csv(newdf,"xxx2.csv",row.names = FALSE)

unique(newdf$BRAND)
summary(lm(logprice~BRAND*logprice,,data=newdf))

gatorade<-df[df$UPC=="5200003925",]
allsport<-df[df$UPC=="1200000757",]
powerade<-df[df$UPC=="4900001923",]

LinearModel.1 <- lm(logmove ~ AGE9 + AGE60 + CPDIST5 
                    + CPWVOL5 + DRTIME5 + EDUC + ETHNIC + Feat + HHLARGE + HHSINGLE + HVAL150 + 
                      INCOME + logprice + NOCAR + NWHITE + POVERTY, data=newdf)
summary(LinearModel.1)

#How does price vary across brands?

barplot(c(mean(powerade$PRICE),mean(gatorade$PRICE),mean(allsport$PRICE)),main="Average price of 32oz Bottle of Each Brand",ylab="Price",xlab="Brand")

mean(powerade$PRICE)
mean(gatorade$PRICE)
mean(allsport$PRICE)

###Allbrand linear model (get price of elsticity)
lm(logmove ~ logprice + Feat + BRAND + logprice*BRAND + AGE9 + AGE60 + ETHNIC + EDUC + NOCAR + INCOME + HHSINGLE + HHLARGE + WORKWOM + HVAL150 + SINGLE + RETIRED + UNEMP + NWHITE + POVERTY, data=newdf)

##logit
GLModel<- glm(Feat ~ BRAND, family=binomial(logit), data=newdf)
summary(GLModel)
exp(coef(GLModel))  # Exponentiated coefficients ("odds ratios")

###
#I = -0.43421 – 1.88755*Gatorade – 0.12890*Powerade
