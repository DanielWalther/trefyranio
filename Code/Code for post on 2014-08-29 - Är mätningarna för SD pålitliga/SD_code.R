## Daniel Walther
# 2014-08-29
# R version 3.1.0 (2014-04-10) -- "Spring Dance"
# Code for special blog post about SD

# Load necessary packages
library(repmis)
library(dplyr)
library(tidyr)
library(ggplot2)
library(car)

# Getting the data
data_url <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls <- repmis::source_data(data_url, sep = ",", dec = ".", header = TRUE)

# Save the data locally as "opinion"
opinion = polls

# Estimating SD averages per polling house
opinion$PublDate = as.Date(opinion$PublDate)

df = filter(opinion, PublDate>as.Date("2013-12-31"))
SDmean = group_by(df, house) %>%
  summarize(SD = mean(SD))

# Calculate standard deviation for all parties and divide by mean
stdev = select(df, M:SD) %>%
  gather(party, result, M:SD) %>%
  group_by(party) %>%
  summarize(sd = sd(result))

avg = select(df, M:SD) %>%
  gather(party, result, M:SD) %>%
  group_by(party) %>%
  summarize(mean = mean(result))

stdev = mutate(stdev, relative_std = (sd/avg$mean))

# Dataset for scatterplot of and time line
# For this to work you need to first run the DLM election forecast
SD = filter(Long, Party=="SD", Date>as.Date("2013-12-31"))
df2 = filter(opinion, PublDate>as.Date("2013-12-31"))


## Dataset with results just before elections
# 2010 Swedish election
runup = filter(opinion, Date>("2010-07-31") & Date<("2010-09-19")) %>%
  mutate(Month = month(Date)) %>%
  mutate(weights = sqrt(n)) %>%
  group_by(Month) %>%
  summarize(Result = round(weighted.mean(SD, weights), 2))

res2010 = c("Valresultat 2010", 5.7)
runup = rbind(runup, res2010)
runup$Election = "2010"
runup$Month = recode(runup$Month, "8='Augusti'")
runup$Month = recode(runup$Month, "9='September'")
runup$Result = as.numeric(runup$Result)

# EU elections data not available on Github yet...
euval = read.table("C:\\Users\\dawa0033\\Dropbox\\Orginal\\Documents\\Documents\\Att förutsäga valet\\Hemsida\\Data för EU-valsinlägg.csv", header=T, sep=";")
SD_EU = filter(euval, Time>1, Parti=="SD")%>%
  mutate(Month = ifelse(Time<5, "April", "Maj"))  %>%
           group_by(Month) %>%
           summarize(Result = round(mean(Poll), 2))

EUres = c("Resultat i EU val", 9.67)
SD_EU = rbind(SD_EU, EUres)
SD_EU$Election = "EU-val"
SD_EU$Result = as.numeric(SD_EU$Result)


## Graphs
# Company means
ggplot(SDmean, aes(x=reorder(house, SD), y=SD,
                   fill=house)) +
  geom_bar(stat="identity", colour="black") +
  labs(x="Institut",
       title="SD:s genomsnittliga resultat hos olika institut under 2014") +
  guides(fill=FALSE) +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 12, 2)) +
  scale_fill_brewer(palette="PuBuGn") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.x=element_text(size=22, face="bold")) + 
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=20, angle=24,
                                 vjust=0.7, face="bold")) +
  theme(axis.text.y=element_text(size=20)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black"))

# Relative standard deviations for each party
ggplot(stdev, aes(x=reorder(party, relative_std) , y=relative_std)) +
  geom_bar(stat="identity", fill="skyblue") +
  theme_bw() +
  ggtitle("Partiernas variation i opinionsresultat\nsom andel av deras medelvärde") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=20, face="bold")) +
  theme(axis.text.y=element_text(size=20, face="bold")) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black"))

         
# Scatterplot and time line

ggplot(SD, aes(x=Date, y=value)) +
  geom_line(size=1.1, colour="goldenrod3") +
  geom_point(size=3, colour="goldenrod3") +
  geom_ribbon(aes(ymin=lb, ymax=ub), alpha=0.2) +
  geom_point(data=df2, aes(x=PublDate, y=SD, colour=house), 
             size=4) +
  labs(y="Resultat (%)", colour="",
       title="Hur har olika institut uppskattat SD:s resultat under 2014?") +
  theme_bw() +
  scale_y_continuous(breaks=seq(8, 13, 1)) +
  scale_x_date(breaks="month") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.x=element_blank()) + 
  theme(legend.text=element_text(size=18)) +
  theme(axis.title.y=element_text(size=22, face="bold")) +
  theme(axis.text.x=element_text(size=20, face="bold")) +
  theme(axis.text.y=element_text(size=20)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black"))

# Polls and actual election result
ggplot(SD_EU, aes(x=Month, y=Result)) +
  geom_bar(stat="identity", fill="skyblue") +
  theme_bw() +
  scale_y_continuous(breaks=seq(0, 10, 2)) +
  ggtitle("EU-valet 2014") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=20, face="bold")) +
  theme(axis.text.y=element_text(size=20, face="bold")) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black"))

ggplot(runup, aes(x=Month, y=Result)) +
  geom_bar(stat="identity", fill="skyblue") +
  theme_bw() +
  ggtitle("Valet 2010") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  scale_y_continuous(breaks=seq(0, 6, 1)) +
  scale_x_discrete(limits=c("Augusti", "September", "Valresultat 2010")) +
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_blank()) +
  theme(axis.text.x=element_text(size=20, face="bold")) +
  theme(axis.text.y=element_text(size=20, face="bold")) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black"))
          
          
