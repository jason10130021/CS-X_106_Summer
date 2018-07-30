# 1.1 load package and data
library("dplyr")
library("data.table")
library("ggplot2")
library("choroplethr")
library("coefplot")
library("lattice")
library("MASS")
colsToKeep <- c("PINCP", "SCHL", "ESR", "ST")
popDataA <- fread("G:/Programming/homework/CS-X_106_Summer/week 3/Data/ss13pusa.csv", select=colsToKeep )  
popDataB <- fread("G:/Programming/homework/CS-X_106_Summer/week 3/Data/ss13pusb.csv", select=colsToKeep )
populData <- rbind(popDataA, popDataB)
rm(popDataA, popDataB)
save(populData, file="populData.RData")

# 1.2 全美無學士文憑、學士、碩士、博士人數比較
ds <-  populData %>%  
  na.omit() %>%
  filter(SCHL %in%  c(18,21,22,24)) %>%
  group_by(SCHL) 
rm(populData)
degreeCode = "SCHL,DegLevel
18,HighSchool
21,Bachelor 
22,Masters
24,Doctorate"
degreCodes <- fread(degreeCode)
degreeHolders <-  summarise(ds,count=n())
degreeHolders <- left_join(degreeHolders , degreCodes, by.x=c("SCHL"))
Degrees <- factor(degreeHolders$DegLevel, levels = unique(degreeHolders$DegLevel))
ggplot(degreeHolders, aes(x= Degrees , y=degreeHolders$count, fill= Degrees)) +   
  geom_bar(stat="identity") + scale_fill_hue(l=40) +
  ylab("Nb. of People") + 
  xlab("Degree") + ggtitle("Comparing Degrees Holders in the US") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))

# 1.3 全美無學士文憑、學士、碩士、博士失業率比較
jobLess <- ds %>%
  filter(ESR==3)%>% #3 indicates  Unemployed
  group_by(SCHL) %>% 
  summarise(count=n())%>%
  mutate(Percet = count/degreeHolders$count*100)
ggplot(jobLess, aes(x=  Degrees, y=Percet, fill= Degrees)) + geom_bar(stat="identity") + scale_fill_hue(l=80) +
  ylab("Percent %") + 
  xlab("Degree") + ggtitle("Percentages of Unemployed Degree  Holders")+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.background = element_rect(fill = 'white' ))

# 1.3 全美無學士文憑、學士、碩士、博士收入比較
aboveThousand <- ds %>%
  filter(PINCP>1000)%>%
  group_by(SCHL) 
freq <- 5000
allStat <- NULL
for(i in 1:freq){
  tempSample <-  sample_n(aboveThousand,1000)
  sampleStat <- summarise(tempSample, MinIncome=min(PINCP), MaxIncome=max(PINCP),
                          MedianIncome=median(PINCP), IncomeRange=IQR(PINCP))  
  allStat <- rbind(allStat, sampleStat)
}
ranSamples <- allStat %>% 
  arrange(SCHL) 

SummaryStat <- ranSamples %>%
  group_by(SCHL)
SummaryStat<- SummaryStat$MinIncome%>% 
  lapply(mean, na.rm = TRUE)
SummaryStat<- SummaryStat$MaxIncome%>% 
  lapply(mean, na.rm = TRUE)
SummaryStat<- SummaryStat$MedianIncome%>% 
  lapply(mean, na.rm = TRUE)
SummaryStat<- SummaryStat$IncomeRange%>% 
  lapply(mean, na.rm = TRUE)
ranSamples <- left_join(ranSamples , degreCodes, by.x=c("SCHL"))
Degrees <- factor(ranSamples$DegLevel, levels = unique(ranSamples$DegLevel))
ggplot(ranSamples, aes(x=Degrees, y=MedianIncome, fill=Degrees ) ) +  
  geom_boxplot(notch = FALSE, outlier.colour="#CC6600") +
  scale_fill_manual(name = "", values = c("#f4a460", "#00BFFF", "#66FF00", "#CC0033")) +
  theme(panel.background = element_rect(fill = 'white' )) +
  ggtitle("Comparing Income of Degrees Holders")  

# 2. T-test 檢驗學歷與收入
# 2.1 高中、學士收入比較的T-test檢驗
testsample<- ranSamples%>%
  filter(SCHL==18 | SCHL==21)
t.test(MedianIncome~SCHL, data=testsample)

# 2.2 學士、博士收入比較的T-test檢驗
testsample<- ranSamples%>%
  filter(SCHL==21 | SCHL==22)
t.test(MedianIncome~SCHL, data=testsample)

# 2.3 碩士與博士收入比較的T-test檢驗
testsample<- ranSamples%>%
  filter(SCHL==24 | SCHL==22)
t.test(MedianIncome~SCHL, data=testsample)

# 3. 利用anova檢定
# 3.1 anova
sample2<- sample_n(aboveThousand,20000)
sample2$degree<- cut(sample2$SCHL, breaks= c(0, 18, 21, 22, 24), labels= c("HighSchool", "Bachelor", "Master", "Doctorate"))
anova(m1 <- lm(PINCP ~ degree, data = sample2))

# 3.2 製圖
ggplot(data = sample2, 
       aes(group = degree, 
           y = PINCP, x = degree)) +
  geom_point() +
  stat_smooth(aes(group = degree, 
                  y = PINCP, x = degree), 
              method = 'lm', se = F) +
  labs(x = '學歷', y = '收入')


m1 <- lm(PINCP ~ degree, data = sample2)
coefplot(m1, xlab = '收入', ylab = '學歷', title = '反應變項 = 收入')

fit_m1 <- data.frame(sample2, fitted = fitted(m1), resid = resid(m1),
                     infl = influence(m1)$hat )
ggplot(data = fit_m1, aes(x = log(PINCP), group = degree )) +
  stat_density(geom = 'path', position = 'identity') +
  stat_density(geom = 'path', position = 'identity', aes(x = fitted)) +
  geom_vline(xintercept = c(with(sample2, tapply(PINCP,degree, mean))), linetype = 'dotted')+
  facet_grid(degree ~ .) +
  scale_x_continuous(breaks = seq(55000, 85000, by = 1000))+
  labs(x = '收入', y = '機率密度')
  
ggplot(data = fit_m1, aes(x = scale(resid)), group = degree ) +
  stat_density(geom = 'path', position = 'identity', aes(linetype = degree)) +
  scale_linetype_manual(values = 5:1) +
  guides(linetype = guide_legend(reverse = TRUE)) +
  labs(x = '標準化殘差', y = '機率密度') +
  theme(legend.position = c(.5, .8))


qqmath(~ scale(resid) | degree, data = fit_m1, type = c('p', 'g', 'r'),
       xlab = '常態位數', ylab = '標準化殘差', layout = c(2, 3),
       pch = '.', cex = 2)


ggplot(data = fit_m1, aes(x = fitted, y = scale(resid), group = degree )) +
  geom_point(pch = 20, size = 1) +
  facet_grid(degree ~ .) +
  labs(x = '收入預測值', y = '標準化殘差')

ggplot(data = fit_m1, aes(x = infl, y = scale(resid), group = degree)) +
  geom_text(aes(label = rownames(fit_m1)), cex = 2) +
  geom_hline(yintercept = 0, linetype = 'dotted') +
  facet_grid(degree ~ .) +
  labs(x = '影響值', y = '標準化殘差')



