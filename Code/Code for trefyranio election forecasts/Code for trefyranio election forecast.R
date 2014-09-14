
## Daniel Walther
## 2014-09-14
## R version 3.1.0 (2014-04-10) -- "Spring Dance"
## Full, replication ready code for the election forecasts on trefyranio.com

## The code is divided into four main sections
# 1. Getting, reorganizing and preparing the data
# 2. Running the dynamic linear model (BSTS package)
# 3. Aggregating results and creating new data sets
# 4. Plotting the results

# Load required packages. If they are not installed, type: 
# install.packages(c("repmis", "stringr", "dplyr", "magrittr", "lubridate", "car", "bsts", "ggplot2", "reshape2", "tidyr", "plyr"))

library(repmis)
library(stringr)
library(magrittr)
library(lubridate)
library(car)
library(bsts)
library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)

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


# Keep only polls from last year 
opinion = filter(opinion, Date>("2013-08-31"))

# First calculate company mean to fill in the missing N values
opinion = plyr::rename(opinion, c("n" = "N"))
compdata = group_by(opinion, Company_code) %>%
  mutate(Nmean = mean(N, na.rm=T))

compdata$N[is.na(compdata$N)] = compdata$Nmean[is.na(compdata$N)]
opinion$N = as.integer(compdata$N)

opinion$N[opinion$house=="Sentio"] = 0.8*opinion$N[opinion$house=="Sentio"]
opinion$N[opinion$house=="Skop"] = 0.5*opinion$N[opinion$house=="Skop"]

# Create new variables
opinion = mutate(opinion, Alliansen = M + FP + C + KD,
                 Left = S + V + MP, 
                 Diff = Left - Alliansen)

# Arrange in ascending order according to date
opinion = arrange(opinion, Date)


### 2. Carry out DLMs for the parties and blocks


# Socialdemokraterna
S = opinion$S
tsS = ts(S)
Sprior = SdPrior(sigma.guess=mean(S[1:4])/50, sample.size=sqrt(opinion$N))


ss = AddLocalLevel(y=S, sigma.prior = Sprior) 

model = bsts(S, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Strend = data.frame(model$state.contributions)
x=colMeans(Strend)
Sdata = data.frame(S=x, Slb=x - 1.96*summary(model)$residual.sd, 
                   Sub=x + 1.96*summary(model)$residual.sd, 
                   date=opinion$Date, house=opinion$house)

# Vänsterpartiet
V = opinion$V
tsV=ts(V)
Vprior = SdPrior(sigma.guess = mean(V)/30,
                 sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=V, sigma.prior = Vprior) 

model = bsts(V, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Vtrend = data.frame(model$state.contributions)
x=colMeans(Vtrend)
Vdata = data.frame(V=x, Vlb=x - 1.96*summary(model)$residual.sd, 
                   Vub=x + 1.96*summary(model)$residual.sd)


# MP
MP = opinion$MP
tsMP=ts(MP)
MPprior = SdPrior(sigma.guess = mean(MP)/30,
                  sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=MP, sigma.prior = MPprior) 

model = bsts(MP, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

MPtrend = data.frame(model$state.contributions)
x=colMeans(MPtrend)
MPdata = data.frame(MP=x, MPlb=x - 1.96*summary(model)$residual.sd, 
                    MPub=x + 1.96*summary(model)$residual.sd)


# Moderaterna
M = opinion$M
tsM=ts(M)
Mprior = SdPrior(sigma.guess = mean(M)/50,
                 sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=M, sigma.prior = Mprior) 

model = bsts(M, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Mtrend = data.frame(model$state.contributions)
x=colMeans(Mtrend)
Mdata = data.frame(M=x, Mlb=x - 1.96*summary(model)$residual.sd, 
                   Mub=x + 1.96*summary(model)$residual.sd)


# Folkpartiet
FP = opinion$FP
tsFP=ts(FP)
FPprior = SdPrior(sigma.guess = mean(FP)/30,
                  sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=FP, sigma.prior = FPprior) 

model = bsts(FP, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

FPtrend = data.frame(model$state.contributions)
x=colMeans(FPtrend)
FPdata = data.frame(FP=x, FPlb=x - 1.96*summary(model)$residual.sd, 
                    FPub=x + 1.96*summary(model)$residual.sd)


# Centern
C = opinion$C
tsC=ts(C)
Cprior = SdPrior(sigma.guess = mean(C)/30,
                 sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=C, sigma.prior = Cprior) 

model = bsts(C, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Ctrend = data.frame(model$state.contributions)
x=colMeans(Ctrend)
Cdata = data.frame(C=x, Clb=x - 1.96*summary(model)$residual.sd, 
                   Cub=x + 1.96*summary(model)$residual.sd)

# Kristdemokraterna
KD = opinion$KD
tsKD=ts(KD)
KDprior = SdPrior(sigma.guess = mean(KD)/30,
                  sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=KD, sigma.prior = KDprior) 

model = bsts(KD, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

KDtrend = data.frame(model$state.contributions)
x=colMeans(KDtrend)
KDdata = data.frame(KD=x, KDlb=x - 1.96*summary(model)$residual.sd, 
                    KDub=x + 1.96*summary(model)$residual.sd 
)

# Sweden Democrats

SD = opinion$SD
tsSD=ts(SD)
SDprior = SdPrior(sigma.guess = mean(SD, na.rm=T)/30,
                  sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=SD, sigma.prior = SDprior) 

model = bsts(SD, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

SDtrend = data.frame(model$state.contributions)
x=colMeans(SDtrend)
SDdata = data.frame(SD=x, SDlb=x - 1.96*summary(model)$residual.sd, 
                    SDub=x + 1.96*summary(model)$residual.sd
)

# Feministiskt initiativ
FIsub = filter(opinion, !is.na(FI))

Fi = FIsub$FI
tsFi=ts(Fi)
Fiprior = SdPrior(sigma.guess = mean(Fi)/30,
                  sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=Fi, sigma.prior = Fiprior) 

model = bsts(Fi, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Fitrend = data.frame(model$state.contributions)
x=colMeans(Fitrend)
Fidata = data.frame(FI=x, FIlb=x - 1.96*summary(model)$residual.sd, 
                    FIub=x + 1.96*summary(model)$residual.sd, 
                    date=FIsub$Date, house=FIsub$house)

# Alliansen
Alliansen = opinion$Alliansen
tsAlliansen=ts(Alliansen)
Alliansenprior = SdPrior(sigma.guess = min(mean(Alliansen)/50, 0.5),
                         sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=Alliansen, sigma.prior = Alliansenprior) 

model = bsts(Alliansen, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Alliansentrend = data.frame(model$state.contributions)
x=colMeans(Alliansentrend)
Alliansendata = data.frame(Alliansen=x, Alliansenlb=x - 1.96*summary(model)$residual.sd, 
                           Alliansenub=x + 1.96*summary(model)$residual.sd, 
                           date=opinion$Date, house=opinion$house)

# Left
Left = opinion$Left
tsLeft=ts(Left)
Leftprior = SdPrior(sigma.guess = min(mean(Left)/50, 0.5),
                    sample.size=sqrt(opinion$N))

ss = AddLocalLevel(y=Left, sigma.prior = Leftprior) 

model = bsts(Left, state.specification = ss, niter=4000, burn=100)
pred <- predict(model, horizon = 12, burn = 100)

Lefttrend = data.frame(model$state.contributions)
x=colMeans(Lefttrend)
Leftdata = data.frame(Left=x, Leftlb=x - 1.96*summary(model)$residual.sd, 
                      Leftub=x + 1.96*summary(model)$residual.sd 
)

### 3. Create new datasets with predictions

# Create joint dataset of predictions
Trends = data.frame(Sdata, Vdata, MPdata, Mdata, FPdata, 
                    Cdata, KDdata, SDdata)


Block_trends = data.frame(Alliansendata, Leftdata)


# Point prediction
Point_data = cbind(Trends[nrow(Trends), ], Fidata[nrow(Fidata),])

Point=melt(Point_data, id="date", measure.vars=c("S", "V", "MP", "M", "FP", "C", "KD",
                                                 "SD", "FI"), variable.name="Party")
Point2=melt(Point_data, id="date", measure.vars=c("Slb", "Vlb", "MPlb", "Mlb", "FPlb", 
                                                  "Clb", "KDlb", "SDlb", "FIlb"), variable.name="Party")
Point3=melt(Point_data, id="date", measure.vars=c("Sub", "Vub", "MPub", "Mub", "FPub", 
                                                  "Cub", "KDub", "SDub", "FIub"), variable.name="Party")

Pred_point=cbind(Point, lb = Point2$value, ub = Point3$value)

#Blocks
Block_point_data = Block_trends[nrow(Block_trends),]

Block_point=melt(Block_point_data, id="date",
                 measure.vars=c("Alliansen", "Left"),variable.name="Block")
Block_point2=melt(Block_point_data, id="date",
                  measure.vars=c("Alliansenlb", "Leftlb"), variable.name="Party")
Block_point3=melt(Block_point_data, id="date", measure.vars=c("Alliansenub", "Leftub"), variable.name="Party")

Block_point=cbind(Block_point, lb = Block_point2$value, ub = Block_point3$value)


# Time trend
Long=melt(Trends, id="date", measure.vars=c("S", "V", "MP", "M", "FP", "C", "KD",
                                            "SD"), variable.name="Party")
Long2=melt(Trends, id="date", measure.vars=c("Slb", "Vlb", "MPlb", "Mlb", "FPlb", 
                                             "Clb", "KDlb", "SDlb"), variable.name="Party")
Long3=melt(Trends, id="date", measure.vars=c("Sub", "Vub", "MPub", "Mub", "FPub", 
                                             "Cub", "KDub", "SDub"), variable.name="Party")
Long=cbind(Long, lb = Long2$value, ub = Long3$value)

# Blocks
Block_long=melt(Block_trends, id="date",
                measure.vars=c("Alliansen", "Left"),variable.name="Block")
Block_long2=melt(Block_trends, id="date",
                 measure.vars=c("Alliansenlb", "Leftlb"), variable.name="Party")
Block_long3=melt(Block_trends, id="date", measure.vars=c("Alliansenub", "Leftub"), variable.name="Party")

Block_long=cbind(Block_long, lb = Block_long2$value, ub = Block_long3$value)



### 4. Plotting the results

# Plot point prediction for each party
ggplot(Pred_point, aes(x=reorder(Party, -value, FUN=mean), y=value)) +
  geom_point(size=6, colour=c("firebrick1", "firebrick3", "seagreen", "royalblue", 
                              "royalblue4", "royalblue1", "purple4", 
                              "goldenrod3", "pink")) +
  geom_errorbar(aes(ymin=lb, ymax=ub), size=1.8, width=0.25,
                colour=c("firebrick1", "firebrick3", "seagreen", "royalblue", 
                         "royalblue4", "royalblue1", "purple4", 
                         "goldenrod3", "pink")) +
  theme_bw() +
  scale_y_continuous(limits=c(0, 40), 
                     breaks=seq(0, 40, 5)) +
  geom_text(aes(label=round(value, digits=1)), hjust=-0.5, size=9) +
  geom_hline(y=4, linetype="dotted", size=0.8) +
  xlab("Parti") +
  ylab("Resultat (%)") +
  ggtitle("Prognos för riksdagsvalet 2014") +
  theme(plot.title=element_text(size=28, face="bold")) +
  theme(axis.title.x=element_text(size=24, vjust=0, face="bold")) + 
  theme(axis.title.y=element_text(size=24, vjust=1, face="bold")) +
  theme(axis.text.x=element_text(size=24, colour=c("firebrick1", "royalblue", "goldenrod3", "seagreen",
                                                   "royalblue4", "firebrick3","royalblue1", "purple4", 
                                                   "pink"), 
                                 face="bold")) +
  theme(axis.text.y=element_text(size=22)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  theme(legend.position="none") +
  annotate("text", label="Fyraprocentsspärren", x=2, y=3, size=7)+
  annotate("text", label="trefyranio.com",
           x=9, y=38, size=7, colour="navajowhite4", 
           fontface="italic")

# Block point prediction
ggplot(Block_point, aes(x=Block, y=value)) +
  geom_point(size=6, colour=c("cornflowerblue", "firebrick3")) +
  geom_errorbar(aes(ymin=lb, ymax=ub), size=1.8, width=0.25, colour=c("cornflowerblue", "firebrick3")) +
  theme_bw() +
  scale_y_continuous(limits=c(30, 55), 
                     breaks=seq(30, 55, 5)) +
  geom_text(aes(label=round(value, digits=1)), size=9, hjust=-0.5) +
  xlab("Block") +
  ylab("Resultat(%)") +
  scale_x_discrete(labels=c("Alliansen", "Vänstersidan")) +
  ggtitle("Prognos för riksdagsvalet för de två blocken") +
  theme(plot.title=element_text(vjust=1, size=28, face="bold")) +
  theme(axis.title.x=element_text(size=24, vjust=0, face="bold")) + 
  theme(axis.title.y=element_text(size=24, vjust=1, face="bold")) +
  theme(axis.text.x=element_text(size=24, colour=c("cornflowerblue", "firebrick3"), 
                                 face="bold")) +
  theme(axis.text.y=element_text(size=24)) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  theme(legend.position="none") +
  annotate("text", label="(SD och Fi ej inkluderade)", x=1.5, y=55, vjust=-1) +
  annotate("text", label="trefyranio.com",
           x=2.4, y=54, size=7, colour="navajowhite4", 
           fontface="italic") 

# Time trend
opinionlong = dplyr::select(opinion, Alliansen, Left) %>%
  gather(Block, Polls, Alliansen:Left)
Block_long2 = cbind(Block_long, Polls = opinionlong$Polls)

ggplot(Block_long2, aes(x=date, y=value, colour=Block)) +
  geom_point(aes(y=Polls), colour="grey", size=3) +
  geom_line(size=1.2) +
  facet_wrap(~Block, scales="free_y") +
  scale_colour_manual(values=c("cornflowerblue", "firebrick3")) +
  ggtitle("DLM-uppskattning och opinionsdata") +
  scale_x_date(breaks="2 months", 
               limits=c(as.Date("2013-10-01"), 
                        as.Date("2014-09-01"))) +
  theme(strip.text=element_text(size=18, face="bold")) +
  theme(axis.title.x=element_blank()) + 
  theme(strip.background=element_rect(fill="skyblue")) +
  labs(y="Resultat") +
  guides(colour=F) +
  theme(axis.title.y=element_text(size=22, vjust=1, face="bold")) +
  theme(axis.text.x=element_text(size=16, vjust=0.5, angle=30)) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold"))

# Party time trend

opinionlong = dplyr::select(opinion, S, V, MP, M, FP, C, KD, SD) %>%
  gather(Party, Polls, S:SD)
Long2 = cbind(Long, Polls = opinionlong$Polls)

ggplot(Long2, aes(x=date, y=value, colour=Party)) +
  geom_point(aes(y=Polls), colour="grey", size=3) +
  geom_line(size=1.2) +
  scale_colour_manual(values=c("firebrick1", "firebrick3", "seagreen",
                               "royalblue", "royalblue4", "royalblue1",
                               "purple4", "goldenrod3")) +    
  facet_wrap(~Party, scales="free_y") +
  scale_x_date(breaks="2 months", 
               limits=c(as.Date("2013-10-13"), 
                        as.Date("2014-09-13"))) +
  ggtitle("DLM-uppskattning och opinionsdata") +
  theme(strip.text=element_text(size=18, face="bold")) +
  theme(axis.title.x=element_blank()) + 
  theme(strip.background=element_rect(fill="skyblue")) +
  labs(y="Resultat") +
  guides(colour=F) +
  theme(axis.title.y=element_text(size=22, vjust=1, face="bold")) +
  theme(axis.text.y=element_text(size=18)) +
  theme(axis.text.x=element_text(size=16, vjust=0.5, angle=30)) +
  theme(plot.title=element_text(vjust=1, size=24, face="bold"))