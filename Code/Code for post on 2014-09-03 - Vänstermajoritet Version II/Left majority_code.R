# Vänstermajoritet? Version II
# Daniel Walther 2014-09-03
# R version 3.1.0 (2014-04-10) -- "Spring Dance"

# To run the code you need to first run the general forecast

# Packages
library(grid)
library(ggplot2)
library(dplyr)

# Get standard deviation and mean of DLM estimate for left side

Blocksub = filter(Block_smooth, Grouping==max(Grouping))
LeftSD = (Blocksub$ubLeft-Blocksub$Left) / 1.96
Leftmean = Blocksub$Left

# recalculate result for a situation where FI and others 
# who don't make it into parliament get 4% of the vote
Leftmean = (Leftmean/96) * 100

# Likelihood that KD gets in parliament
partysub = filter(smooth, Grouping==max(Grouping))
KDSD = (partysub$ubKD-partysub$KD) / 1.96
KDmean = partysub$KD
Likelihood_noKD = 1 - pnorm(4, KDmean, KDSD, lower.tail=F)

# Conditional probability of Left majority given KD's result
ResnoKD = Blocksub$Left/92.1
Leftmean = Leftmean +  ResnoKD*Likelihood_noKD

# Create normal distribution of likelihoods of those values
Leftdist = dnorm(seq(45, 55, 0.1), Leftmean, LeftSD)
LeftDF = data.frame(likelihood = Leftdist, xvalues=seq(45, 55, 0.1))
likelihood_Leftmaj = pnorm(49.9, Leftmean, LeftSD, lower.tail=F)

# Plot the result
ggplot(LeftDF, aes(x=xvalues, y=likelihood)) +
  geom_line() +
  scale_x_continuous(breaks=seq(45, 55, 1)) +
  theme_bw() +
  geom_area(data=subset(LeftDF, xvalues>49.9), alpha=0.5, fill="firebrick3") +
  labs(x="Resultat", y="",
       title="Hur sannolikt är det att vänstersidan får egen majoritet?") +
  theme(axis.text.y=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.text.x=element_text(size=18)) +
  theme(axis.title.x=element_text(size=20)) +
  theme(plot.title=element_text(size=24, face="bold")) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  geom_vline(xintercept=50, linetype="dashed") +
  annotate("text", label="Sannolikhet att vänstersidan får\n en majoritet av mandaten efter valet=49.7%",
           x=53, y=0.25, size=7) +
  annotate("segment", x = 52, y = 0.23, xend = 51.5, yend = 0.21,
           arrow = arrow(length = unit(0.5, "cm")), size=1.5, colour="firebrick3") +
  annotate("text", label="49.9%", x=49.9, y=0.34, size=7, colour="firebrick3")

