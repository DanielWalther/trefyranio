# Code for trefyranio.com
# Daniel Walther, 2014-09-10
# R version 3.1.0 (2014-04-10) -- "Spring Dance"

library(tidyr)
library(dplyr)
library(ggplot2)

# Reshape polling data for plotting

polls2 = opinion %>%
  gather(Party, Resultat, M:SD)


# Graph: Are som parties more difficult to measure?
# Long is the dataset with DLM predictions

ggplot(Long, aes(x=date, y=value)) +
  geom_line(size=1) +
  geom_ribbon(aes(ymin=lb, ymax=ub),
              alpha=0.25, colour=element_blank()) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=45, size=20, hjust=1)) +
  theme(axis.text.y=element_text(size=20)) +
  geom_point(data=polls2, aes(x=Date, y=Resultat, colour=house), 
             size=3) +
  facet_wrap(~Party, scales="free_y") +
  scale_x_date(breaks="3 months", 
               limits=c(as.Date("2013-10-01"), 
                        as.Date("2014-09-01"))) +
  xlab("") +
  ylab("Stöd (%)") +
  ggtitle("Är vissa partier svårare att mäta?") +
  scale_colour_brewer(palette="Set1") +
  theme(strip.text=element_text(face="bold", size=18)) +
  theme(plot.title=element_text(vjust=1, size=28, face="bold")) +
  theme(axis.title.y=element_text(size=24, face="bold", vjust=0.6)) +
  theme(panel.grid.major.y=element_line(size=1)) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(strip.background=element_rect(fill="skyblue")) +
  theme(legend.position=c(0.8, 0.15))

# Create data sets with smaller parties

smaller6 = filter(Long, value<18)
polls3=filter(polls2, Resultat<18)

# Graph: Polling houses and the smaller parties
ggplot(smaller6, aes(x=date, y=value, colour=Party)) +
  geom_line(size=1) +
  theme_bw() +
  theme(axis.text.y=element_text(size=20)) +
  geom_point(data=polls3, aes(x=Date, y=Resultat, colour=Party), 
             size=3) +
  facet_wrap(~house, scales="free_y") +
  scale_x_date(breaks="3 months", 
               limits=c(as.Date("2013-10-01"), 
                        as.Date("2014-09-01"))) +
  xlab("") +
  ylab("Stöd (%)") +
  ggtitle("Opinionsdata från de olika instituten för de sex mindre partierna") +
  scale_colour_manual(values=c("red2", "seagreen", 
                               "royalblue4", "royalblue1", "purple4", 
                               "darkgoldenrod1")) +
  scale_fill_grey() +
  theme(strip.text=element_text(face="bold", size=18)) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.text.x=element_text(size=18, angle=30, vjust=0.5)) +
  theme(axis.title.y=element_text(size=18, face="bold", vjust=0.4)) +
  theme(panel.grid.major.y=element_line(size=1)) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(strip.background=element_rect(fill="skyblue"))

# Create new data sets for larger parties
large2 = filter(Long, value>18)
polls4=filter(polls2, Resultat>18)

# Graph: Polling houses and larger parties
ggplot(large2, aes(x=date, y=value, colour=Party)) +
  geom_line(size=1) +
  theme_bw() +
  theme(axis.text.y=element_text(size=20)) +
  theme(axis.text.x=element_text(size=18, angle=30, vjust=0.5)) +
  geom_point(data=polls4, aes(x=Date, y=Resultat, colour=Party), 
             size=3) +
  facet_wrap(~house, scales="free_y") +
  scale_x_date(breaks="3 months", 
               limits=c(as.Date("2013-10-01"), 
                        as.Date("2014-09-01"))) +
  xlab("") +
  ylab("Stöd (%)") +
  ggtitle("Opinionsdata från de olika instituten för S och M") +
  scale_colour_manual(values=c("red2", "skyblue")) +
  scale_fill_grey() +
  theme(strip.text=element_text(face="bold", size=18)) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.y=element_text(size=18, face="bold", vjust=0.4)) +
  theme(panel.grid.major.y=element_line(size=1)) +
  theme(legend.title=element_blank()) +
  theme(legend.text=element_text(size=20)) +
  theme(strip.background=element_rect(fill="skyblue"))