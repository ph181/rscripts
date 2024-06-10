library(lsr)


groups <- Erhebung
groups <- Erhebung[which(Erhebung$ C== 1),]


t.test(groups$SV, groups$SN, paired = TRUE, alternative = "two.sided")
cohensD(groups$SV, groups$SN, method="paired")

t.test(groups$FV, groups$FN, paired = TRUE, alternative = "two.sided")
cohensD(groups$FV, groups$FN, method="paired")

#wilcox signed rank
wilcox.test(group$FV, group$FN, mu = 0, alternative = 'two.sided')
wilcox.test(group$SV, group$SN, mu = 0, alternative = 'two.sided')


#wilcox.test(group$FV,group$FN, paired = TRUE, exact = TRUE, correct = TRUE, conf.int = TRUE)





