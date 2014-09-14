# Final predictions
# Daniel Walther 2014-09-14
# R version 3.1.0 (2014-04-10) -- "Spring Dance"

library(dplyr)
library(ggplot2)

# To run the code you need to first run the general forecast

# Select the data
CKDFI = Pred_point %>% 
  filter (Party=="C" | Party=="KD" |
     Party=="FI") %>%
  mutate(sd = (ub-value)/1.96)

C = filter(CKDFI, Party=="C")
KD = filter(CKDFI, Party=="KD")
Fi = filter(CKDFI, Party=="FI")

# Generate normal data with same qualities
# as data in CKDFI

Cnorm = dnorm(seq(5, 7.4, 0.1), C$value, C$sd)
Cnorm = data.frame(Cnorm, xvalues=seq(5, 7.4, 0.1))
KDnorm = dnorm(seq(4.2, 6.1, 0.1), KD$value, 
               KD$sd)
KDnorm = data.frame(KDnorm, xvalues=seq(4.2, 6.1, 0.1))
Finorm = dnorm(seq(1.9, 4.6, 0.1), Fi$value, 
               Fi$sd)
Finorm = data.frame(Finorm, xvalues=seq(1.9, 4.6, 0.1))



# Graph results
ggplot(Cnorm, aes(x=xvalues, y=Cnorm)) +
  geom_ribbon(aes(ymin=0.09, ymax=Cnorm),
              fill="royalblue3", alpha=0.7, 
            colour="black") +
  geom_ribbon(data=KDnorm,
              aes(x=xvalues, y=KDnorm, 
                  ymin=0.09, ymax=KDnorm), 
            fill="purple4", alpha=0.7) +
  geom_line(data=Finorm, aes(x=xvalues, 
                             y=Finorm)) +
  geom_ribbon(data=filter(Finorm, 
                        xvalues>3.99), 
            aes(x=xvalues, y=Finorm, 
                ymin=0.09, ymax=Finorm),
            fill="pink", alpha=0.7) +
  scale_x_continuous(breaks=seq(2, 7, 1)) +
  labs(x="Resultat (%)", y="",
       title="Sannolikhetsfördelning för\n C, KD och Fi") +
  theme_bw() +
  theme(axis.text.y=element_blank()) +
  theme(panel.grid.major.x=element_blank()) +
  theme(axis.text.x=element_text(size=18)) +
  theme(axis.title.x=element_text(size=20)) +
  theme(plot.title=element_text(size=24, face="bold")) +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major=element_line(size=1)) +
  theme(panel.border=element_blank()) +
  theme(axis.line=element_line( colour="black")) +
  geom_vline(xintercept=4, linetype="dashed") +
  annotate("text", label="6.2", x=6.2, y=0.7) +
  annotate("text", label="5.2", x=5.15, y=0.9) +
  annotate("text", label="3.3", x=3.3, y=0.65)
  
# Probability that the left gets a majority

Leftsub = Block_point %>%
  filter(Block=="Left") %>%
  mutate(sd = (ub-value)/1.96)

# Result if Fi and others get 4% of the vote 
# or if Fi gets in
IncreasewithFi = (Leftsub$value+Fi$value) - Leftsub$value
Increasenofi = ((Leftsub$value/96)*100) - Leftsub$value

# Likelihood of that happening
likelihood_Fi = pnorm(4, Fi$value, Fi$sd, lower.tail=F)
likelihood_noFi= 1- pnorm(4, Fi$value, Fi$sd, lower.tail=F)

# Total probable result
Leftsub = mutate(Leftsub, 
                 total = value + IncreasewithFi*likelihood_Fi +
                   Increasenofi*likelihood_noFi)

Leftnorm = data.frame(Result = dnorm(seq(44, 51, 0.1),
                                      Leftsub$total, Leftsub$sd),
                      xvalues=seq(44, 51, 0.1))

Likelihood_majority = pnorm(50, Leftsub$total, Leftsub$sd, 
                            lower.tail=F)

# Graph
ggplot(Leftnorm, aes(x=xvalues, y=Result)) +
  geom_line() +
  scale_x_continuous(breaks=seq(44, 53, 1)) +
  theme_bw() +
  geom_area(data=subset(Leftnorm, xvalues>49.9),
            alpha=0.5, fill="firebrick3") +
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
  annotate("text", label="47.8", x=47.8, y=0.34) +
  annotate("text", label="4%", x=50.5, y=0.06, 
           colour="firebrick3")

