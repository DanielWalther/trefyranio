## Daniel Walther
## 2014-09-01
## R version 3.1.0 (2014-04-10) -- "Spring Dance"
## FUll, replication ready code for the election forecasts on trefyranio.com

## The code is divided into four main sections
# 1. Getting, reorganizing and weighting the data
# 2. Running the dynamic linear model (DLM package)
# 3. Aggregating results and creating new data sets
# 4. Plotting the results

# Load required packages. If they are not installed, type: 
# install.packages(c("repmis", "stringr", "dplyr", "magrittr", "lubridate", "car", "dlm", "ggplot2", "reshape2", "tidyr", "plyr"))

library(repmis)
library(stringr)
library(dplyr)
library(magrittr)
library(lubridate)
library(car)
library(dlm)
library(ggplot2)
library(reshape2)
library(tidyr)


#### 1. Getting the data and creating a weighted time series
data_url <- "https://github.com/MansMeg/SwedishPolls/raw/master/Data/Polls.csv"
polls <- repmis::source_data(data_url, sep = ",", dec = ".", header = TRUE)

# Save the data locally as "opinion"
opinion = polls

# Creating index for polling institutes
opinion$house = as.factor(opinion$house)
opinion$Company_code = as.numeric(opinion$house)

## Create meaningful date and month variables

opinion = plyr::rename(opinion, c("PublDate" = "Date"))
opinion$Date = as.Date(opinion$Date)

opinion = plyr::rename(opinion, c("PublYearMonth" = "Month_code"))
opinion$Month_code = str_replace_all(opinion$Month_code, "-", "")

opinion = mutate(opinion, 
                 Month_code = str_replace_all(Month_code, "-", ""), 
                 Month_code = str_replace_all(Month_code,"jan", "01"), 
                 Month_code = str_replace_all(Month_code,"feb", "02"), 
                 Month_code = str_replace_all(Month_code,"mar", "03"),
                 Month_code = str_replace_all(Month_code,"march", "03"),
                 Month_code = str_replace_all(Month_code,"april", "04"),
                 Month_code = str_replace_all(Month_code,"apr", "04"),
                 Month_code = str_replace_all(Month_code,"may", "05"),
                 Month_code = str_replace_all(Month_code,"maj", "05"),
                 Month_code = str_replace_all(Month_code,"jun", "06"),
                 Month_code = str_replace_all(Month_code,"june", "06"),
                 Month_code = str_replace_all(Month_code,"jul", "07"),
                 Month_code = str_replace_all(Month_code,"aug", "08"),
                 Month_code = str_replace_all(Month_code,"sept", "09"),
                 Month_code = str_replace_all(Month_code,"sep", "09"),
                 Month_code = str_replace_all(Month_code,"okt", "10"),
                 Month_code = str_replace_all(Month_code,"oct", "10"),
                 Month_code = str_replace_all(Month_code,"nov", "11"),
                 Month_code = str_replace_all(Month_code,"dec", "12"), 
                 Month_code = paste(Month_code, 15, sep=""), 
                 Month_code = ymd(Month_code))

# Replace missing values in Date with mid-month estimates from Month_code
opinion$Date[is.na(opinion$Date)] = opinion$Month_code[is.na(opinion$Date)]
opinion$Month_code=NULL

## Remove comment signs if you want to use a different year than 2014
#2006
#opinion = filter(opinion, Date>("2005-10-30") & Date<("2006-09-17"))

##2010
#opinion = filter(opinion, Date>("2009-10-30") & Date<("2010-09-19"))

##2014
opinion = filter(opinion, Date>("2012-10-31"))

## Weighting the polls 
# First calculate company mean to fill in the missing N values
opinion = plyr::rename(opinion, c("n" = "N"))
compdata = group_by(opinion, Company_code) %>%
  mutate(Nmean = mean(N, na.rm=T))

compdata$N[is.na(compdata$N)] = compdata$Nmean[is.na(compdata$N)]
opinion$N = as.integer(compdata$N)

# Create indicator for every 8000 people asked
opinion = arrange(opinion, Date) %>%
  mutate(totN=cumsum(N),
         grouping = (floor(totN/8000)+1))

#Root(n) cohort totals

opinion = mutate(opinion, rootn = sqrt(N)) %>%
  group_by(grouping) %>%
  mutate(Tn = sum(rootn, na.rm=T))


# Turn estimated votes into proportions
opinion = mutate(opinion, share_M = M/100, 
                 share_FP = FP/100, 
                 share_C = C/100, 
                 share_KD = KD/100,
                 share_C = C/100, 
                 share_S = S/100,
                 share_V = V/100,
                 share_MP = MP/100, 
                 share_SD = SD/100, 
                 share_FI = FI/100, 
                 share_Uncertain = Uncertain/100)


# Weigth by poll size
opinion = mutate(opinion, weighted_M = share_M*sqrt(N), 
                 weighted_FP = share_FP*sqrt(N), 
                 weighted_C = share_C*sqrt(N), 
                 weighted_KD = share_KD*sqrt(N),
                 weighted_S = share_S*sqrt(N), 
                 weighted_V = share_V*sqrt(N), 
                 weighted_MP = share_MP*sqrt(N), 
                 weighted_SD = share_SD*sqrt(N), 
                 weighted_FI = share_FI*sqrt(N), 
                 weighted_Uncertain = share_Uncertain*sqrt(N))

# Sum the observations for each grouping
group = group_by(opinion, grouping) %>%
  mutate(tot_M = sum(weighted_M, na.rm=T),
         tot_FP = sum(weighted_FP, na.rm=T),
         tot_C = sum(weighted_C, na.rm=T),
         tot_KD = sum(weighted_KD, na.rm=T),
         tot_S = sum(weighted_S, na.rm=T),
         tot_V = sum(weighted_V, na.rm=T),
         tot_MP = sum(weighted_MP, na.rm=T),
         tot_SD = sum(weighted_SD, na.rm=T),
         tot_FI = sum(weighted_FI, na.rm=T),
         tot_Uncertain = sum(weighted_Uncertain, na.rm=T))

opinion = cbind(opinion, group[, -10:-1])

# Calculate weighted vote shares per month
opinion = mutate(opinion, wshare_M = tot_M/Tn, 
                 wshare_FP = tot_FP/Tn, 
                 wshare_C = tot_C/Tn,
                 wshare_KD = tot_KD/Tn,
                 wshare_S = tot_S/Tn,
                 wshare_V = tot_V/Tn,
                 wshare_MP = tot_MP/Tn,
                 wshare_SD = tot_SD/Tn,
                 wshare_FI = tot_FI/Tn,
                 wshare_Uncertain = tot_Uncertain/Tn)

# Create one row per grouping now that the scores have been aggregated
opinion = mutate(opinion, 
                 m = month(Date), 
                 year = year(Date), 
                 month = paste(year, m, sep="-"))
opinion = opinion[!duplicated(opinion$grouping),]
opinion = arrange(opinion, grouping)

# Replace with weighted shares and return to %
opinion = mutate(opinion, M = round(wshare_M*100, 2), 
                 FP = round(wshare_FP*100, 2), 
                 C = round(wshare_C*100, 2),
                 KD = round(wshare_KD*100, 2),
                 S = round(wshare_S*100, 2),
                 V = round(wshare_V*100, 2),
                 MP = round(wshare_MP*100, 2),
                 SD = round(wshare_SD*100, 2),
                 FI = round(wshare_FI*100, 2),
                 Uncertain = round(wshare_Uncertain*100, 2))

opinion = mutate(opinion, Allians = M + FP + C + KD,
                 Left = S + V + MP, 
                 Diff = Left - Allians)

# Keep only columns of interest
opinion = select(opinion, grouping, M, FP, C, KD, S, V, MP,   
                 SD, FI, Uncertain, month,
                 Allians, Left, Diff, Tn)



#### 2. Running the dynamic linear models

# Variance in trends is deemed to be 3 times greater than measured
# estimates due to bias in the polls. For Fi and the blocks the observation
# level variance is taken to be a factor seven of the latent state variance.

DLMdata = opinion

# SOCIALDEMOKRATERNA
tsS = ts(DLMdata$S)

# Set up model (random walk plus noise)
ModdlmS = dlmModPoly(order=1, 
                     dV=(StructTS(tsS, type="level"))$coef[2]*3, 
                     dW=(StructTS(tsS, type="level"))$coef[1]*3, 
                     m0=mean(DLMdata$S[1:4]), 
                     C0=var(DLMdata$S))


# Kalmar Filter
ModfiltS = dlmFilter(DLMdata$S, 
                     mod=ModdlmS, 
                     simplify=FALSE)

# Smooth
dlmsS = dlmSmooth(ModfiltS)

# Forecast
predictS = dlmForecast(ModfiltS, 
                       nAhead=2, 
                       method="svd", 
                       sampleNew=5)


# VÄNSTERPARTIET
tsV = ts(DLMdata$V)

# Set up model (random walk plus noise)
ModdlmV = dlmModPoly(order=1,
                     dV=(StructTS(tsV, type="level"))$coef[2]*2, 
                     dW=(StructTS(tsV, type="level"))$coef[1]*2,
                     m0=mean(DLMdata$V[1:4]),
                     C0=var(DLMdata$V))


# Kalman filter
ModfiltV = dlmFilter(DLMdata$V, mod=ModdlmV)


# Smooth
dlmsV = dlmSmooth(ModfiltV)

# Forecast
predictV = dlmForecast(ModfiltV, nAhead=2, method="svd", sampleNew=5)

# MILJÖPARTIET
tsMP = ts(DLMdata$MP)
fitMP=dlmMLE(tsMP, parm=c(100, 2), build)
fitMP$convergence

# Set up model (random walk plus noise)
ModdlmMP = dlmModPoly(order=1,
                      dV=(StructTS(tsMP, type="level"))$coef[2]*3, 
                      dW=(StructTS(tsMP, type="level"))$coef[1]*3,
                      m0=mean(DLMdata$MP[1:4]),
                      C0=var(DLMdata$MP))

# Kalman filter
ModfiltMP = dlmFilter(DLMdata$MP, mod=ModdlmMP)

# Smooth
dlmsMP = dlmSmooth(ModfiltMP)


# Forecast
predictMP = dlmForecast(ModfiltMP, nAhead=2, method="svd", sampleNew=5)


# MODERATERNA
tsM = ts(DLMdata$M)

# Set up model (random walk plus noise)
ModdlmM = dlmModPoly(order=1,
                     dV=(StructTS(tsM, type="level"))$coef[2]*3, 
                     dW=(StructTS(tsM, type="level"))$coef[1]*3,
                     m0=mean(DLMdata$M[1:4]),
                     C0=var(DLMdata$M))

# Kalman filter
ModfiltM = dlmFilter(DLMdata$M, mod=ModdlmM)

# Smooth
dlmsM = dlmSmooth(ModfiltM)

# Forecast
predictM = dlmForecast(ModfiltM, nAhead=2, method="svd", sampleNew=5)


# FOLKPARTIET
tsFP = ts(DLMdata$FP)

# Set up model (random walk plus noise)
ModdlmFP = dlmModPoly(order=1,
                      dV=(StructTS(tsFP, type="level"))$coef[2]*3, 
                      dW=(StructTS(tsFP, type="level"))$coef[1]*3,
                      m0=mean(DLMdata$FP[1:4]),
                      C0=var(DLMdata$FP))

# Kalman filter
ModfiltFP = dlmFilter(DLMdata$FP, mod=ModdlmFP)

# Smooth
dlmsFP = dlmSmooth(ModfiltFP)

# Forecast
predictFP = dlmForecast(ModfiltFP, nAhead=2, method="svd", sampleNew=5)


# CENTERPARTIET
tsC = ts(DLMdata$C)

# Set up model (random walk plus noise)
ModdlmC = dlmModPoly(order=1,
                     dV=(StructTS(tsC, type="level"))$coef[2]*3, 
                     dW=(StructTS(tsC, type="level"))$coef[1]*3,
                     m0=mean(DLMdata$C[1:4]),
                     C0=var(DLMdata$C))

# Kalman filter
ModfiltC = dlmFilter(DLMdata$C, mod=ModdlmFP)

# Smooth
dlmsC = dlmSmooth(ModfiltC)


# Forecast
predictC = dlmForecast(ModfiltC, nAhead=2, method="svd", sampleNew=5)



# KRISTDEMOKRATERNA
tsKD = ts(DLMdata$KD)

# Set up model (random walk plus noise)
ModdlmKD = dlmModPoly(order=1,
                      dV=(StructTS(tsKD, type="level"))$coef[2]*3, 
                      dW=(StructTS(tsKD, type="level"))$coef[1]*3,
                      m0=mean(DLMdata$KD[1:4]),
                      C0=var(DLMdata$KD))

# Kalman filter
ModfiltKD = dlmFilter(DLMdata$KD, mod=ModdlmKD)

# Smooth
dlmsKD = dlmSmooth(ModfiltKD)


# Forecast
predictKD = dlmForecast(ModfiltKD, nAhead=2, method="svd", sampleNew=5)


# SVERIGEDEMOKRATERNA
tsSD = ts(DLMdata$SD)

# Set up model (random walk plus noise)
ModdlmSD = dlmModPoly(order=1,
                      dV=(StructTS(tsSD, type="level"))$coef[2]*3, 
                      dW=max((StructTS(tsSD, type="level"))$coef[1], 0.07)*3,
                      m0=mean(DLMdata$SD[1:4]),
                      C0=var(DLMdata$SD)*4)

# Kalman filter
ModfiltSD = dlmFilter(DLMdata$SD, mod=ModdlmSD)

# Smooth
dlmsSD = dlmSmooth(ModfiltSD)


# Forecast
predictSD = dlmForecast(ModfiltSD, nAhead=2, method="svd", sampleNew=5)


# FI
tsFi = ts(DLMdata$FI)


# Set up model (random walk plus noise)
ModdlmFi = dlmModPoly(order=1,
                      dV=max((StructTS(tsFi, type="level"))$coef[2], 0.05)*3, 
                      dW=(StructTS(tsFi, type="level"))$coef[1]*3,
                      m0=mean(DLMdata$FI[28:32]),
                      C0=var(DLMdata$FI, na.rm=T))

# Kalman filter
ModfiltFi = dlmFilter(DLMdata$FI, mod=ModdlmFi)

# Smooth
dlmsFi = dlmSmooth(ModfiltFi)


# Forecast
predictFi = dlmForecast(ModfiltFi, nAhead=2, method="svd", sampleNew=5)


## The Blocks

# Alliansen
tsAlli = ts(DLMdata$Allians)

# Set up model (random walk plus noise)
ModdlmAlli = dlmModPoly(order=1,
                        dV=(StructTS(tsAlli, type="level"))$coef[2]*3, 
                        dW=(StructTS(tsAlli, type="level"))$coef[1]*3,
                        m0=mean(DLMdata$Allians[1:4]),
                        C0=var(DLMdata$Allians))

# Kalman filter
ModfiltAlli = dlmFilter(DLMdata$Allians, mod=ModdlmAlli)

# Smooth
dlmsAlli = dlmSmooth(ModfiltAlli)


# Forecast
predictAlli = dlmForecast(ModfiltAlli, nAhead=2, method="svd", sampleNew=5)


# Vänster
tsLeft = ts(DLMdata$Left)

# Set up model (random walk plus noise)
ModdlmLeft = dlmModPoly(order=1,
                        dV=(StructTS(tsLeft, type="level"))$coef[2]*3, 
                        dW=(StructTS(tsLeft, type="level"))$coef[1]*3,
                        m0=mean(DLMdata$Left[1:4]),
                        C0=var(DLMdata$Left))
# Kalman filter
ModfiltLeft = dlmFilter(DLMdata$Left, mod=ModdlmLeft)

# Smooth
dlmsLeft = dlmSmooth(ModfiltLeft)

# Forecast
predictLeft = dlmForecast(ModfiltLeft, nAhead=2, method="svd", sampleNew=5)


#### 3. Create data frame with estimates
# Smooth trend and a prediction for one month ahead is captured
# To account for the fact that there probably is some bias in the polls
# we can't measure we calculate 99% CI intervals and treat them as
# 95% intervals

attach(dlmsS)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbS = s-(2.57*sd)
ubS=s+(2.57*sd)
smooth = data.frame(cbind(s, lbS, ubS))
smooth = plyr::rename(smooth, c("s"="S"))
detach(dlmsS)

attach(dlmsV)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbV = s-(2.57*sd)
ubV=s+(2.57*sd)
smooth = cbind(smooth, s, lbV, ubV)
smooth = plyr::rename(smooth, c("s"="V"))
detach(dlmsV)

attach(dlmsMP)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbMP = s-(2.57*sd)
ubMP=s+(2.57*sd)
smooth = cbind(smooth, s, lbMP, ubMP)
smooth = plyr::rename(smooth, c("s"="MP"))
detach(dlmsMP)

attach(dlmsM)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbM = s-(2.57*sd)
ubM=s+(2.57*sd)
smooth = cbind(smooth, s, lbM, ubM)
smooth = plyr::rename(smooth, c("s"="M"))
detach(dlmsM)

attach(dlmsFP)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbFP = s-(2.57*sd)
ubFP=s+(2.57*sd)
smooth = cbind(smooth, s, lbFP, ubFP)
smooth = plyr::rename(smooth, c("s"="FP"))
detach(dlmsFP)

attach(dlmsC)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbC = s-(2.57*sd)
ubC=s+(2.57*sd)
smooth = cbind(smooth, s, lbC, ubC)
smooth = plyr::rename(smooth, c("s"="C"))
detach(dlmsC)

attach(dlmsKD)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbKD = s-(2.57*sd)
ubKD=s+(2.57*sd)
smooth = cbind(smooth, s, lbKD, ubKD)
smooth = plyr::rename(smooth, c("s"="KD"))
detach(dlmsKD)

attach(dlmsSD)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbSD = s-(2.57*sd) 
ubSD=s+(2.57*sd)
smooth = cbind(smooth, s, lbSD, ubSD)
smooth = plyr::rename(smooth, c("s"="SD"))
detach(dlmsSD)

attach(dlmsFi)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbFi = s-(2.57*sd)
ubFi=s+(2.57*sd)
smooth = cbind(smooth, s, lbFi, ubFi)
smooth = plyr::rename(smooth, c("s"="Fi"))
detach(dlmsFi)

smooth = smooth[-1, ] # Remove row of starting values
smooth = round(smooth, 2)
smooth = cbind(smooth, Month=(DLMdata$month), 
               Grouping = DLMdata$grouping)

attach(dlmsAlli)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbAlli = s-(2.57*sd)
ubAlli=s+(2.57*sd)
Block_smooth = data.frame(cbind(s, lbAlli, ubAlli))
Block_smooth = plyr::rename(Block_smooth, c("s"="Alliansen"))
detach(dlmsAlli)

attach(dlmsLeft)
sd=sqrt(unlist(dlmSvd2var(U.S, D.S)))
lbLeft = s-(2.57*sd)
ubLeft=s+(2.57*sd)
Block_smooth = cbind(Block_smooth, s, lbLeft, ubLeft)
Block_smooth = plyr::rename(Block_smooth, c("s"="Left"))
detach(dlmsLeft)

Block_smooth = Block_smooth[-1, ]
Block_smooth = round(Block_smooth, 2)
Block_smooth = cbind(Block_smooth, Month=(DLMdata$month), 
                     Grouping = DLMdata$grouping)



## Reorganize data for plotting with ggplot

# The individual parties
Smooth_long=melt(smooth, id=c("Grouping", "Month"), measure.vars=c("S", "V", "MP", "M", "FP", "C", "KD",
                                                                   "SD", "Fi"), variable.name="Party")
Smooth_long2=melt(smooth, id="Grouping", measure.vars=c("lbS", "lbV", "lbMP", "lbM", "lbFP", 
                                                        "lbC", "lbKD", "lbSD", "lbFi"), variable.name="Party")
Smooth_long3=melt(smooth, id="Grouping", measure.vars=c("ubS", "ubV", "ubMP", "ubM", "ubFP", 
                                                        "ubC", "ubKD", "ubSD", "ubFi"), variable.name="Party")
Smooth_long=cbind(Smooth_long, lb = Smooth_long2$value, ub = Smooth_long3$value)

Pred_point = filter(Smooth_long, Grouping==max(Smooth_long$Grouping))

# The two blocks
Block_smooth_long=melt(Block_smooth, id=c("Grouping", "Month"), measure.vars=c("Alliansen", "Left"), variable.name="Block")
Block_smooth_long2=melt(Block_smooth, id="Grouping", measure.vars=c("lbAlli", "lbLeft"), variable.name="Block")
Block_smooth_long3=melt(Block_smooth, id="Grouping", measure.vars=c("ubAlli", "ubLeft"), variable.name="Block")
Block_smooth_long=cbind(Block_smooth_long, lb=Block_smooth_long2$value, ub=Block_smooth_long3$value)

Block_point = filter(Block_smooth_long, Grouping==max(Block_smooth_long$Grouping))

## 4. Plotting
# Plot point prediction for each party
ggplot(Pred_point, aes(x=reorder(Party, -value, FUN=mean), y=value)) +
  geom_point(size=5, colour=c("firebrick1", "firebrick3", "seagreen", "royalblue", 
                              "royalblue4", "royalblue1", "purple4", 
                              "goldenrod3", "pink")) +
  geom_errorbar(aes(ymin=lb, ymax=ub), size=1.6, width=0.2,
                colour=c("firebrick1", "firebrick3", "seagreen", "royalblue", 
                         "royalblue4", "royalblue1", "purple4", 
                         "goldenrod3", "pink")) +
  theme_bw() +
  scale_y_continuous(limits=c(0, 40), 
                     breaks=seq(0, 40, 5)) +
  geom_text(aes(label=round(value, digits=2)), hjust=-0.5, size=7) +
  geom_hline(y=4, linetype="dotted", size=0.8) +
  xlab("Parti") +
  ylab("Resultat (%)") +
  ggtitle("Prognos för riksdagsvalet 2014") +
  theme(plot.title=element_text(size=24, face="bold")) +
  theme(axis.title.x=element_text(size=22, face="bold")) + 
  theme(axis.title.y=element_text(size=22, vjust=0.8, face="bold")) +
  theme(axis.text.x=element_text(size=22, colour=c("firebrick1", "royalblue", "seagreen", "goldenrod3",
                                                   "firebrick3", "royalblue4",  "royalblue1", "purple4",
                                                   "pink"), 
                                 face="bold")) +
  theme(axis.text.y=element_text(size=20)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  theme(legend.position="none") +
  annotate("text", label="Fyraprocentsspärren", x=2, y=3, size=7)+
  annotate("text", label="", x=8, y=37,
           size=6, colour="navajowhite4")

# Point prediction for the blocks
ggplot(Block_point, aes(x=Block, y=value)) +
  geom_point(size=5, colour=c("cornflowerblue", "firebrick3")) +
  geom_errorbar(aes(ymin=lb, ymax=ub), size=1.5, width=0.2, colour=c("cornflowerblue", "firebrick3")) +
  theme_bw() +
  scale_y_continuous(limits=c(30, 55), 
                     breaks=seq(30, 55, 5)) +
  geom_text(aes(label=round(value, digits=2)), size=7, hjust=-0.5) +
  xlab("Block") +
  ylab("Resultat(%)") +
  scale_x_discrete(labels=c("Alliansen", "Vänstersidan")) +
  ggtitle("Prognos för riksdagsvalet för de två blocken") +
  theme(plot.title=element_text(vjust=1, size=24, face="bold")) +
  theme(axis.title.x=element_text(size=22, face="bold")) + 
  theme(axis.title.y=element_text(size=22, vjust=1, face="bold")) +
  theme(axis.text.x=element_text(size=20, colour=c("cornflowerblue", "firebrick3"), 
                                 face="bold")) +
  theme(axis.text.y=element_text(size=20)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  theme(legend.position="none") +
  annotate("text", label="(SD och Fi ej inkluderade)", x=1.5, y=55, vjust=-1) +
  annotate("text", label="", x=2.3, y=53,
           size=6, colour="navajowhite4")


# Graph model fit to observed data points
DLMlong = select(DLMdata, S, V, MP, M, FP, C, KD, SD, FI) %>%
  gather(Party, Polls, S:FI)
Smooth_long = cbind(Smooth_long, Polls = DLMlong$Polls)

ggplot(Smooth_long, aes(x=Grouping, y=value, colour=Party)) +
  geom_line() +
  geom_point(colour="grey") +
  geom_point(aes(x=Grouping, y=Polls, colour=Party)) +
  facet_wrap(~Party, scales="free_y") +
  ggtitle("DLM trend line and observed data points") +
  theme(axis.title.x=element_text(size=22, face="bold")) + 
  theme(axis.title.y=element_text(size=22, vjust=1, face="bold")) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold"))