library(readr)
#most recent:
data <- as.data.frame(read_csv("MarieCurie.csv"))


#grab question descriptions
qs <- unlist(c(data[1,]))
head(qs)
qs[1]

#remove qualtrics first rows
data <- data[-1,]
data <- data[-1,]

#get rid of the testing rows
#View(data[1:12,])
data <- data[13:nrow(data),]#starting with feb 28

library(visdat)
#vis_miss(data[data$Finished=="True",])

data <- data[data$Finished=="TRUE",]#keep only the ones that finished

sum(data$Status=="Survey Preview")
data <- data[data$Status!="Survey Preview",]


#n completes
nrow(data)


#check on durations and remove extreme outliers
data$duration <- as.numeric(data$`Duration (in seconds)`)
hist(data$duration[data$duration<=10e3], 70, 
     xlab="Duration (seconds)",
     main="Histogram of Survey Response Durations")

quantile(data$duration, .05)
240/60#throw out if faster than 4 minutes
abline(v=240, lwd=3, lty=3); abline(v=36e2, lwd=3, lty=3)
240/60#four minutes
data <- data[data$duration>240,]
#data <- data[data$duration<36e2,]#not run, only remove if too fast
10/858

#now how many completes
nrow(data)

#DATA RANGE OF RESPONSES
range(data$StartDate)

#Demographics
table(data$ideology)
data$ideology <- factor(data$ideology, levels=c("Extremely conservative", "Conservative", "Somewhat conservative", "Neither conservative nor liberal", "Somewhat liberal", "Liberal", "Extremely liberal"))
table(data$ideology)
data$ideology <- as.numeric(data$ideology)
table(data$ideology)

table(data$gender)/sum(table(data$gender))

data$age <- 2019 - as.numeric(data$year_born)
hist(data$age, 30, xlab="Age", main="Histogram of Age")
mean(data$age, na.rm = T)
sd(data$age, na.rm = T)

#education
table(data$education)
sum(is.na(data$education))

#climate work
table(data$climate_work)/sum(table(data$climate_work))


######################################process policy support


#aggregate policy support
table(data$climate_policysuppor_1)
data$climate_policysuppor_1 <- factor(data$climate_policysuppor_1, levels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support"))
table(data$climate_policysuppor_1)
data$climate_policysuppor_1 <- as.numeric(data$climate_policysuppor_1)
table(data$climate_policysuppor_1)
sum(table(data$climate_policysuppor_1))

table(data$climate_policysuppor_2)
data$climate_policysuppor_2 <- factor(data$climate_policysuppor_2, levels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support"))
table(data$climate_policysuppor_2)
data$climate_policysuppor_2 <- as.numeric(data$climate_policysuppor_2)
table(data$climate_policysuppor_2)
sum(table(data$climate_policysuppor_2))

table(data$climate_policysuppor_3)
data$climate_policysuppor_3 <- factor(data$climate_policysuppor_3, levels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support"))
table(data$climate_policysuppor_3)
data$climate_policysuppor_3 <- as.numeric(data$climate_policysuppor_3)
table(data$climate_policysuppor_3)
sum(table(data$climate_policysuppor_3))

table(data$climate_policysuppor_4)
data$climate_policysuppor_4 <- factor(data$climate_policysuppor_4, levels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support"))
table(data$climate_policysuppor_4)
data$climate_policysuppor_4 <- as.numeric(data$climate_policysuppor_4)
table(data$climate_policysuppor_4)
sum(table(data$climate_policysuppor_4))

#climate policy support aggregated
data$climate_policy_support <- rowMeans(data[,c("climate_policysuppor_1","climate_policysuppor_2","climate_policysuppor_3","climate_policysuppor_4")], na.rm = F)#na.rm=F so we only take a mean when they answered all questions

hist(data$climate_policy_support,20)


######################################analyze worries


worry <- data[,c("climate_worry", "pollution_worry", "job_worry", "economy_worry", "other_worry", "immigration_worry", "terrorism_worry", "crt")]

head(worry); nrow(worry)

for(c in 1:(ncol(worry)-1))
{
  worry[,c] <- factor(worry[,c], levels=c("Not at all worried","Not very worried", "Somewhat worried", "Very worried", "Extremely worried"))
}
head(worry)

worry_num <- as.data.frame(sapply(worry, as.numeric))#numeric version

pairs(sapply(worry_num, jitter))

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

png("processed/graphs/Worries Scatterplot Matrix.PNG", width=1000, height=1000)
pairs(sapply(worry_num, jitter),
      lower.panel=panel.smooth, upper.panel=panel.cor, 
      pch=20, main="Worries Scatterplot Matrix")
dev.off()

library(corrplot)
png("processed/graphs/Worries Correlation Matrix.PNG", width=1000, height=1000)
corrplot.mixed(cor(worry_num, use="complete.obs"), upper="color")
dev.off()


library(ggplot2); library(tidyr)
worry_long <- gather(worry, threat, worry, climate_worry:terrorism_worry)
worry_counts <- as.data.frame(table(worry_long$worry, worry_long$threat))
head(worry_counts)
class(worry_counts$Var1)
worry_counts$Var1 <- factor(worry_counts$Var1, levels=c("Not at all worried","Not very worried",   "Somewhat worried", "Very worried", "Extremely worried"))
levels(worry_counts$Var1) 
head(worry_counts)

#graph the named worries
worry_counts <- worry_counts[worry_counts$Var2!="other_worry",]
worry_counts$Var2 <- gsub("_worry", "", worry_counts$Var2)
worry_counts$Var2 <- ifelse(worry_counts$Var2=="job", "Unemployment",
                            worry_counts$Var2)

firstup <- function(x) {substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x}#fix capitalization

worry_counts$Var2 <- firstup(worry_counts$Var2)
worry_counts$Var2 <- factor(worry_counts$Var2, levels=c("Climate", "Pollution", "Economy", "Unemployment", "Terrorism", "Immigration"))

library(ggsci); library(ggplot2)
ggplot(worry_counts, aes(x=Var1, y=Freq, fill=Var2, alpha=.5)) + geom_bar(stat="identity") + facet_wrap(facets=vars(Var2))+  theme_bw() + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5), text=element_text(size=20)) + xlab("") +ggtitle("Self-reported Worry About Threats")+
  scale_fill_manual(values=c("green1", "darkgreen",
                             "deepskyblue1", "navy", 
                             "orange", "darkorange3"))+
  theme(plot.title = element_text(hjust = 0.5)) + facet_grid(cols=vars(Var2)) 

ggsave("FINALGRAPHS/Worry Histograms.PNG", height=5, width=12)


head(worry_long)
worry_long$worry <- factor(worry_long$worry, levels=c("Not at all worried","Not very worried", "Somewhat worried", "Very worried", "Extremely worried"))
worry_long$threat <- gsub("_worry", "", worry_long$threat)

class(worry_long$worry)
levels(worry_long$worry)
worry_long$numerical_worry <- as.numeric(worry_long$worry)


#proportion worried about each
head(worry_long)
worry_long$worried <- ifelse(worry_long$worry=="Somewhat worried" | worry_long$worry=="Very worried" | worry_long$worry=="Extremely worried", 1, 0)
worry_long$threat <- ifelse(worry_long$threat=="job", "unemployment", worry_long$threat)
worry_long$threat <- firstup(worry_long$threat)
worry_long <- worry_long[worry_long$threat!="Other",]
worry_long$threat <- as.factor(worry_long$threat)

agg <- aggregate(worried ~ threat, worry_long, FUN="mean")
agg
head(agg)

#SEs 
sigmas <- sqrt(agg$worried*(1-agg$worried))
SEs <- sigmas/sqrt(nrow(data))
SEs
CIs <- SEs * 1.96
CIs


head(agg)
#for colors
agg$threat <- factor(agg$threat, levels=c("Climate", "Pollution", "Economy", "Unemployment", "Terrorism", "Immigration"))

library(ggplot2)
ggplot(agg, aes(x=threat, y=worried, fill=threat)) + geom_bar(stat="identity")   + theme_bw() +#coord_flip() 
  xlab("") + ylab("Proportion worried about each issue")+ theme(legend.position = "none", text=element_text(size=20), plot.title = element_text(hjust = 0.5)) +ggtitle("Proportion of participants worried about different issues")  +# geom_jitter(data=worry_long, aes(x=threat, y=numerical_worry/5), color="grey", height=.1, width=.19, alpha=.5) 
#+
  scale_fill_manual(values=c("green1", "darkgreen",
                             "deepskyblue1", "navy", 
                             "orange", "darkorange3"))+ geom_errorbar(aes(ymin=agg$worried-CIs, ymax=agg$worried+CIs), col="black", width=.2)

agg$ses <- SEs
agg$upper <- agg$worried + CIs
agg$lower <- agg$worried - CIs
agg

ggsave("FINALGRAPHS/Percent worried.PNG", height=7, width=12)
write.csv(agg, "FINALGRAPHS/Percent worried.csv")


######################################analyze policy support

#wide to long
all_policies <- gather(data[,c("ResponseId", "climate_policysuppor_1", "climate_policysuppor_2", "climate_policysuppor_3", "climate_policysuppor_4")], policy_name, response, climate_policysuppor_1:climate_policysuppor_4, factor_key=TRUE)
table(all_policies$policy_name)
head(all_policies)
all_policies$policy_name <- as.character(all_policies$policy_name)

#put in actual policy names
qs[65:68]

aggregate(response ~ policy_name, all_policies, FUN="mean")#should match below

all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_1", "Fund more research into renewable energy sources", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_2", "Regulate CO2 as a pollutant", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_3", "Phase out gasoline and diesel fueled cars", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_4", "Require fossil fuel companies to pay a carbon tax", all_policies$policy_name)

aggregate(response ~ policy_name, all_policies, FUN="mean")


library(stringr)
counts <- as.data.frame(table(all_policies$response, all_policies$policy_name))
head(counts)
unique(counts$Var1) 
table(counts$Var1) 
unique(counts$Var2)

counts <- counts[!is.na(counts$Var1),]

unique(counts$Var2)
table(counts$Var2)

counts$Var2 <- factor(counts$Var2, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
table(counts$Var2)
library(ggplot2)
ggplot(counts, aes(x=Var1, y=Freq, fill=Var2, alpha=.5)) + geom_bar(stat="identity") + facet_wrap(facets=vars(Var2), ncol=2) + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + xlab("")+ scale_x_discrete(labels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support")) + ggtitle("Climate Policy Support") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+scale_fill_npg() + facet_grid(rows=vars(rev(Var2)))
ggsave("FINALGRAPHS/Climate Policy Support.PNG", height=10.2, width=5)

all_policies <- all_policies[complete.cases(all_policies),]
write.csv(all_policies, "processed/policy_support_questions.csv", row.names=F)


#percentage of approval per policy
table(all_policies$response)
#=1 if somewhat support or higher
all_policies$approve <- ifelse(all_policies$response>=4, 1, 0)
agg <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="mean"))
agg

#SEs 
sigmas <- sqrt(agg$approve*(1-agg$approve))
SEs <- sigmas/sqrt(nrow(data))
SEs
CIs <- SEs * 1.96
CIs


agg$policy_name <- factor(agg$policy_name, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
ggplot(agg, aes(x=policy_name, y=approve, fill=policy_name)) + geom_bar(stat="identity") + coord_flip() + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + ylab("Proportion that support each policy")+ theme(legend.position = "none") +theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+ ggtitle("Proportion of participants supporting climate policies")+scale_fill_npg()+ geom_errorbar(aes(ymin=agg$approve-CIs, ymax=agg$approve+CIs), col="black", width=.2)
ggsave("FINALGRAPHS/Climate Policy Support bar.PNG", height=8, width=14)

agg$ses <- SEs
agg$upper <- agg$approve + CIs
agg$lower <- agg$approve - CIs
agg
write.csv(agg, "FINALGRAPHS/Climate Policy Support bar.csv")


######################################policy support by ideology


#only liberals
library(tidyr)
all_policies <- gather(data[data$ideology>=5,c("ResponseId", "climate_policysuppor_1", "climate_policysuppor_2", "climate_policysuppor_3", "climate_policysuppor_4")], policy_name, response, climate_policysuppor_1:climate_policysuppor_4, factor_key=TRUE)
table(all_policies$policy_name)
head(all_policies)
all_policies$policy_name <- as.character(all_policies$policy_name)

#put in actual policy names
qs[65:68]


aggregate(response ~ policy_name, all_policies, FUN="mean")#should match below

all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_1", "Fund more research into renewable energy sources", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_2", "Regulate CO2 as a pollutant", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_3", "Phase out gasoline and diesel fueled cars", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_4", "Require fossil fuel companies to pay a carbon tax", all_policies$policy_name)

aggregate(response ~ policy_name, all_policies, FUN="mean")


library(stringr)
#!D#all_policies$policy_name <- str_wrap(all_policies$policy_name)

counts <- as.data.frame(table(all_policies$response, all_policies$policy_name))
# counts$Var1 <- factor(counts$Var1, levels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support"))
counts
unique(counts$Var1) 
table(counts$Var1) 
unique(counts$Var2)

counts <- counts[!is.na(counts$Var1),]

unique(counts$Var2)
table(counts$Var2)

counts$Var2 <- factor(counts$Var2, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
table(counts$Var2)
library(ggplot2)
ggplot(counts, aes(x=Var1, y=Freq, fill=Var2, alpha=.5)) + geom_bar(stat="identity") + facet_wrap(facets=vars(Var2), ncol=2) + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + xlab("")+ scale_x_discrete(labels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support")) + ggtitle("Climate Policy Support") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+scale_fill_npg() + facet_grid(rows=vars(rev(Var2)))
# ggsave("FINALGRAPHS/Climate Policy Support - liberals.PNG", height=10.2, width=5)


#percentage of approval per policy
table(all_policies$response)
#1 if somewhat support or higher
all_policies$approve <- ifelse(all_policies$response>=4, 1, 0)
agg <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="mean"))
agg

library(binom)
agg_1 <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="sum"));agg_1
agg_n <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="length"));agg_n
get_lower <- function(r){binom.confint(agg_1$approve[r], agg_n$approve[r])$lower[1]}#agresti-coull
agg$upper_AC <- sapply(1:4, get_upper); agg
get_upper <- function(r){binom.confint(agg_1$approve[r], agg_n$approve[r])$upper[1]}#agresti-coull
agg$lower_AC <- sapply(1:4, get_lower); agg
agg$upper_AC <- sapply(1:4, get_upper); agg


#SEs 
sigmas <- sqrt(agg$approve*(1-agg$approve))
SEs <- sigmas/sqrt(nrow(data[data$ideology>=5,]))
SEs
CIs <- SEs * 1.96
CIs
agg$SEs <- SEs

agg$policy_name <- factor(agg$policy_name, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
liberals <- agg
ggplot(agg, aes(x=policy_name, y=approve, fill=policy_name)) + geom_bar(stat="identity") + coord_flip() + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + ylab("Proportion that support each policy")+ theme(legend.position = "none") +theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+ ggtitle("Proportion of participants supporting climate policies")+scale_fill_npg()+ geom_errorbar(aes(ymin=agg$approve-CIs, ymax=agg$approve+CIs), col="black", width=.2)
# ggsave("FINALGRAPHS/Climate Policy Support bar liberals.PNG", height=8, width=14)


###########


#only conservatives
all_policies <- gather(data[data$ideology<5,c("ResponseId", "climate_policysuppor_1", "climate_policysuppor_2", "climate_policysuppor_3", "climate_policysuppor_4")], policy_name, response, climate_policysuppor_1:climate_policysuppor_4, factor_key=TRUE)
table(all_policies$policy_name)
head(all_policies)
all_policies$policy_name <- as.character(all_policies$policy_name)

#put in actual policy names
qs[65:68]


aggregate(response ~ policy_name, all_policies, FUN="mean")

all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_1", "Fund more research into renewable energy sources", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_2", "Regulate CO2 as a pollutant", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_3", "Phase out gasoline and diesel fueled cars", all_policies$policy_name)
all_policies$policy_name <- ifelse(all_policies$policy_name == "climate_policysuppor_4", "Require fossil fuel companies to pay a carbon tax", all_policies$policy_name)

aggregate(response ~ policy_name, all_policies, FUN="mean")


library(stringr)
counts <- as.data.frame(table(all_policies$response, all_policies$policy_name))
head(counts)
unique(counts$Var1) 
table(counts$Var1) 

unique(counts$Var2)

counts <- counts[!is.na(counts$Var1),]

unique(counts$Var2)
table(counts$Var2)

counts$Var2 <- factor(counts$Var2, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
table(counts$Var2)
library(ggplot2)
ggplot(counts, aes(x=Var1, y=Freq, fill=Var2, alpha=.5)) + geom_bar(stat="identity") + facet_wrap(facets=vars(Var2), ncol=2) + theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + xlab("")+ scale_x_discrete(labels=c("Strongly oppose", "Somewhat oppose", "Neither support nor oppose", "Somewhat support", "Strongly support")) + ggtitle("Climate Policy Support") +
  theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+scale_fill_npg() + facet_grid(rows=vars(rev(Var2)))
# ggsave("FINALGRAPHS/Climate Policy Support - conservatives.PNG", height=10.2, width=5)


#percentage of approval per policy
table(all_policies$response)
#1 if somewhat support or higher
all_policies$approve <- ifelse(all_policies$response>=4, 1, 0)
agg <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="mean"))
agg

#better yet, get binomial SE from binom calculations
library(binom)
agg_1 <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="sum"));agg_1
agg_n <- as.data.frame(aggregate(approve ~ policy_name, all_policies, FUN="length"));agg_n
get_lower <- function(r){binom.confint(agg_1$approve[r], agg_n$approve[r])$lower[1]}#agresti-coull
agg$upper_AC <- sapply(1:4, get_upper); agg
get_upper <- function(r){binom.confint(agg_1$approve[r], agg_n$approve[r])$upper[1]}#agresti-coull
agg$lower_AC <- sapply(1:4, get_lower); agg
agg$upper_AC <- sapply(1:4, get_upper); agg


#SEs 
sigmas <- sqrt(agg$approve*(1-agg$approve))
SEs <- sigmas/sqrt(nrow(data[data$ideology<5,]))
SEs
CIs <- SEs * 1.96
CIs
agg$SEs <- SEs


agg$policy_name <- factor(agg$policy_name, levels=c("Fund more research into renewable energy sources", "Regulate CO2 as a pollutant", "Phase out gasoline and diesel fueled cars", "Require fossil fuel companies to pay a carbon tax"))
conservatives <- agg
ggplot(agg, aes(x=policy_name, y=approve, fill=policy_name)) + geom_bar(stat="identity") + coord_flip() + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + ylab("Proportion that support each policy")+ theme(legend.position = "none") +theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+ ggtitle("Proportion of participants supporting climate policies")+scale_fill_npg()+ geom_errorbar(aes(ymin=agg$approve-CIs, ymax=agg$approve+CIs), col="black", width=.2)
# ggsave("FINALGRAPHS/Climate Policy Support bar conservatives.PNG", height=8, width=14)


liberals$class <- "Liberals"
conservatives$class <- "Non-liberals"
agg <- rbind(liberals, conservatives);agg
agg$upper <- agg$approve+(1.96*agg$SEs)
agg$lower <- agg$approve-(1.96*agg$SEs)

library(ggsci)
ggplot(agg, aes(x=policy_name, y=approve, fill=class)) + geom_bar(stat="identity", position="dodge") + coord_flip() + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) + ylab("Proportion that support each policy")+theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+ ggtitle("Proportion of participants supporting climate policies")+scale_fill_npg()+
  geom_errorbar(aes(ymin=agg$lower, ymax=agg$upper), col="black", width=.2, position=position_dodge(width=1)) #+
#  geom_errorbar(aes(ymin=agg$lower_AC, ymax=agg$upper_AC), col="magenta", width=.2, position=position_dodge(width=1))
ggsave("FINALGRAPHS/Climate Policy Support bar liberals v conservatives.PNG", height=8, width=14)
write.csv(agg, "FINALGRAPHS/support_by_ideology.csv")


#another approach, same results:
names(data)
data$fund_more <- ifelse(data$climate_policysuppor_1>=4,1,0)
data$lib_cons <- ifelse(data$ideology<4, "conservative", NA)
data$lib_cons <- ifelse(data$ideology==4, "moderate", data$lib_cons)
data$lib_cons <- ifelse(data$ideology>4, "liberal", data$lib_cons)

means <- aggregate(fund_more ~ lib_cons, data, FUN="mean")
  colnames(means) <- c("lib_cons", "mean")
ns <- aggregate(fund_more ~ lib_cons, data, FUN="length")
  colnames(ns) <- c("lib_cons", "n")
sds <- aggregate(fund_more ~ lib_cons, data, FUN="sd")
  colnames(sds) <- c("lib_cons", "sd")

three <- merge(means, ns, by="lib_cons")
three <- merge(three, sds, by="lib_cons")
three$upper <- three$mean + 1.96*(three$sd/sqrt(three$n))
three$lower <- three$mean - 1.96*(three$sd/sqrt(three$n))
three

# ggplot(three, aes(x=lib_cons, y=mean)) + geom_bar(stat="identity", position="dodge") + coord_flip() + xlab("") + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=.5)) +theme(plot.title = element_text(hjust = 0.5), text=element_text(size=20))+ ggtitle("Proportion of participants supporting funding renewables")+
#   geom_errorbar(aes(ymin=three$lower, ymax=three$upper), col="black", width=.2, position=position_dodge(width=1)) 



data$fund_more <- ifelse(data$climate_policysuppor_1>=4,1,0)
aggregate(fund_more ~ lib_cons, data, FUN="mean")

#check the significance of the differences between the support of the two groups
#http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
#fund renew
x <- data$climate_policysuppor_1[data$ideology>=5]; head(x)
y <- data$climate_policysuppor_1[data$ideology<5]; head(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

#regulate co2 as poll
x <- data$climate_policysuppor_2[data$ideology>=5]; head(x)
y <- data$climate_policysuppor_2[data$ideology<5]; head(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

#phase out fossil fuel cars
x <- data$climate_policysuppor_3[data$ideology>=5]; head(x)
y <- data$climate_policysuppor_3[data$ideology<5]; head(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

#carbon tax
x <- data$climate_policysuppor_4[data$ideology>=5]; head(x)
y <- data$climate_policysuppor_4[data$ideology<5]; head(y)
t.test(x, y, alternative = "two.sided", var.equal = FALSE)

table(data$ideology)
39/sum(table(data$ideology))


######################################visualize maps

library(sf)
library(ggspatial)#this allows adding points from just lat lons

world <- st_as_sf(rnaturalearth::countries110)
head(world)
world_countries <- unique(world$name)
europe <- dplyr::filter(world, region_un=="Europe" & name!='Russia')
#europe <- world

# A bounding box for continental Europe.
europe.bbox <- st_polygon(list(
  matrix(c(-25,29,45,29,45,75,-25,75,-25,29),byrow = T,ncol = 2)))


europe.clipped <- st_intersection(europe, st_sfc(europe.bbox, crs=st_crs(europe)))
head(europe.clipped)
countries <- unique(europe.clipped$name)

#resolve ambiguities
data$country2 <- data$country
data$country2[!(tolower(data$country2) %in% tolower(countries))]
data$country2[toupper(data$country2) == "UK" | toupper(data$country2) == "SCOTLAND" | toupper(data$country2) == "ENGLAND"] <- "United Kingdom"
data$country2[toupper(data$country2) == "THE NETHERLANDS"] <- "Netherlands"
data$country2[toupper(data$country2) == "USA"] <- "United States"
data$country2[data$country2 == "Czech Republic" | data$country2 == "Czechia"] <- "Czech Rep."
data$country2[!(tolower(data$country2) %in% tolower(countries))]
data$country2[!(tolower(data$country2) %in% tolower(world_countries) | is.na(data$country2))]

sum(tolower(data$country2) %in% tolower(world_countries) | is.na(data$country2))/nrow(data)

table <- as.data.frame(table(toupper(data$country2)))
colnames(table) <- c("name", "subject_count")

europe.clipped$name <- toupper(europe.clipped$name)
world$name <- toupper(world$name)

europe.clipped <- merge(europe.clipped, table, by="name", all.x=T)
world <- merge(world, table, by="name", all.x=T)
head(europe.clipped); head(world)

#just europe
library(ggplot2)
ggplot(europe.clipped, aes(fill=subject_count)) +
  geom_sf(alpha=0.8,col='white')+
  #layer_spatial(sites)  +
  coord_sf(crs="+proj=aea +lat_1=36.333333333333336 +lat_2=65.66666666666667 +lon_0=14") +
  hrbrthemes::theme_ipsum_rc() +  #viridis::scale_fill_viridis(name='Number of Participants \nPer Country', direction = -1) +
  scale_fill_gradient(low = "gold", high = "darkorange", name='Number of Participants \nPer Country')+#scale_fill_manual(values=c("white", "blue")) +#scale_fill_continuous() +
  labs(x=NULL, y=NULL, title=NULL,
       caption='') + theme(legend.position = "bottom")
ggsave("FINALGRAPHS/euro_map_of_Ps.png", width=8.5, height=6)


#world map
library(ggplot2)
ggplot(world, aes(fill=subject_count)) +
  geom_sf(alpha=0.8,col='white')+
  #layer_spatial(sites)  +
  coord_sf(crs=3857) +
  hrbrthemes::theme_ipsum_rc() +  scale_fill_gradient(low = "gold", high = "darkorange", name='Number of Participants \nPer Country') +
  #viridis::scale_fill_viridis(name='Number of Participants \nPer Country', direction = -1) +
  labs(x=NULL, y=NULL, title=NULL,
       caption='') + theme(legend.position = "bottom")
ggsave("FINALGRAPHS/world_map_of_Ps.png", width=8.5, height=8.5)