#Federally-facilitated Marketplace (FFM)
#State-based-facilitated Marketplace (SBM)
install.packages('likert')
library(DataComputing)
library (XML)
library(dplyr)
library (tidyr)
library(ggplot2)
library(plotrix)
library(plyr)
library(lubridate)

getwd()


data_set <- read.csv(file = "data set.csv")
names(data_set)

### metal level circular bar plot ###
a <- sum(data_set['Bronze'])
b <- sum(data_set['Silver'])
c <- sum(data_set['Gold'])
d <- sum(data_set['Platinum'], na.rm = TRUE)
e <- sum(data_set['Catastrophic'])

metal_level = data.frame(group =c('Bronze', 'Silver', 'Gold', 'Platinum', 'Catastrophic'), 
                         value = c(a, b, c, d, e))
bar <- ggplot(metal_level, aes(x = group, y = value, fill=group))+geom_bar(width = 0.85, stat="identity")+coord_polar(theta = "y") +xlab("") + ylab("") +ylim(c(0,20000000))
bar <- bar + ggtitle("Bar Plot for Metal Level in both SBM and FFM Marketplace")
bar <- bar + geom_text(data = metal_level, hjust = 1, size = 3, aes(x = group, y = 0, label = group))
bar <- bar + theme(legend.position = "none" , axis.text.y = element_blank() , axis.ticks = element_blank())

### Age Level Bar Plot ###
fe_18<-sum(data_set["Female.below.18"], na.rm = TRUE)
fe_18_25<-sum(data_set['Female18.25'], na.rm = TRUE)
fe_26_34<-sum(data_set['Female.26.34'], na.rm = TRUE)
fe_35_44<-sum(data_set['Female.35.44'], na.rm = TRUE)
fe_45_55<-sum(data_set['Female.45.55'], na.rm = TRUE)
fe_55_64<-sum(data_set['Female.55.64'], na.rm = TRUE)
fe_64<-sum(data_set['Female.over.65'], na.rm = TRUE)

m_18<-sum(data_set["Male.below.18"], na.rm = TRUE)
m_18_25<-sum(data_set['Male18.25'], na.rm = TRUE)
m_26_34<-sum(data_set['Male.26.34'], na.rm = TRUE)
m_35_44<-sum(data_set['Male.35.44'], na.rm = TRUE)
m_45_55<-sum(data_set['Male.45.55'], na.rm = TRUE)
m_55_64<-sum(data_set['Male.55.64'], na.rm = TRUE)
m_64<-sum(data_set['Male.over.65'], na.rm = TRUE)

#xy.pop<-c(fe_18, fe_18_25, fe_26_34, fe_35_44, fe_45_55, fe_55_64, fe_64)
#xx.pop<-c(m_18, m_18_25, m_26_34, m_35_44, m_45_55, m_55_64, m_64)
xx.pop<-c(3.47832, 6.55026, 10.02370, 10.20536, 13.58190, 15.81246, .22765)
xy.pop<-c(3.63176, 5.54770, 8.63940, 8.59850, 10.79076, 11.62240, .18887)
agelabels<-c('Age over 18', 'Age between 18 and 25', 'Age between 26 and 34',
             'Age over 35 and 44', 'Age 45 and 54', 'Age 55 and 64', 'Age over 65')

#mcol<-color.gradient(c(0,0,0.5,1),c(0,0,0.5,1),c(1,1,0.5,1),7)
#fcol<-color.gradient(c(1,1,0.5,1),c(0.5,0.5,0.5,1),c(0.5,0.5,0.5,1),7)
mcol<-c("#FFF0F5", "#FFB6C1", "#FFAEB9", "#EEA2AD", "#CD8C95", "#8B5F65", '#8B475D')
fcol<-c("#E0FFFF", "#BBFFFF", "#00FFFF", "#00EEEE", "#96CDCD", "#668B8B", "#2F4F4F")

par(mar=pyramid.plot(xy.pop,xx.pop,labels=agelabels,
                     main="Pyramid Plot for Marketplace Coverage by Gender and Age",lxcol=mcol,rxcol=fcol,
                     gap=5,show.values=TRUE))


### SBM & FFM ###

# Create test data.
data_set %>% 
  group_by(MarketType) %>%
  summarise(no_rows = length(MarketType))
dat = data.frame(count=c(15, 37), category=c("SBM", "FFM"))

# Add addition columns, needed for drawing with geom_rect.
dat$fraction = dat$count / sum(dat$count)
dat = dat[order(dat$fraction), ]
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

# Make the plot
p1 = ggplot(dat, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect() +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank()) +
  annotate("text", x = 0, y = 0, label = "Marketplace by SBM and FFM") +
  labs(title="")
p1

### Medicade & CHIP ###
proportion <- data_set["Medicaid.CHIP"]/data_set["TotalEligible"]
state <- data_set['StateName']
CHIP_Medi <- data.frame(proportion, state = state)
colnames(CHIP_Medi) <- c("proportion", "state")
prop_plot <- ggplot(data=CHIP_Medi, aes(x=state, y=proportion)) +
  geom_bar(stat="identity") +
  theme(axis.text.x=element_text(angle=90))

##### Excluded: Kaiser hospitals, state mental hospitals, 
####  psychiatric health facilities, and hospitals with mainly long-term care patients 
####  Deductions from Revenue, Net Patient Revenue, Net from Operations 
#### (Operating Revenue less Operating Expense), and Net Income for public hospitals

profit_set <- read.csv(file = "Hospital_Profitability__2009-2013.csv")
names(profit_set)
profit_set <- profit_set[!is.na(profit_set$Income.Statement.Amount), ]

a<-profit_set %>%
  group_by(Year, Type.of.Control) %>%
  summarise(n = sum(Income.Statement.Amount, na.rm = TRUE))
print (a)
plot <- ggplot(a, aes(x=Year,y=n/100000000)) + 
  geom_point(aes(col = Type.of.Control)) + geom_line(aes(col = Type.of.Control))+
  ylim(c(0, 8000))+ geom_vline(xintercept=2010)+labs(title="Profit for Hospital in CA 2009-2013",x="Year",y="Profit (Hundred Million)")
plot


############ Health Poll ############
tracking <- read.csv('Health.csv')
names(tracking)
bar <- tracking %>%
  select(Date, Favorable, Unfavorable, Don.t.Know) %>%
  gather (key = Attitude, value, Favorable, Unfavorable, Don.t.Know)
ggplot(bar, aes(x=Date, y=value, fill = Attitude)) + geom_bar(stat="identity") + theme(axis.text.x=element_text(angle=90))

######### Attitudes towards ACA against Age, Party, Income #########
regression<-tracking %>%
  select(Democrat_Favorable, Democrat_Unfavorable, Democrat_Don.t.Know, Independent_Favorable, Independent_Unfavorable, Independent_Don.t.know, Republican_Don.t.know, Republican_Unfavorable, Republican_Don.t.know)%>%
  gather(key = Party, Attitude, Democrat_Favorable, Democrat_Unfavorable, Democrat_Don.t.Know, Independent_Favorable, Independent_Unfavorable, Independent_Don.t.know, Republican_Don.t.know, Republican_Unfavorable, Republican_Don.t.know)
model <- lm(Attitude~Party, data=regression)
summary(model)































