#
install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)


plot(Erhebung$V1,Erhebung$V2)


as.factor(Erhebung$Group)


one.way <- aov(V3 ~ V2, data = Erhebung)

summary(one.way)

two.way <- aov(V3 ~ V2 + V1 +V4, data = Erhebung)

summary(two.way)

interaction <- aov(V3 ~ V2*V1, data = Erhebung)

summary(interaction)

blocking <- aov(V3 ~ V2 + V1 + V4, data = Erhebung)

summary(blocking)

model.set <- list(one.way, two.way, interaction, blocking)
model.names <- c("one.way", "two.way", "interaction", "blocking")

aictab(model.set, modnames = model.names)



#check for homoscedasticity
par(mfrow=c(2,2))
plot(two.way)
par(mfrow=c(1,1))

#posthoc Tukey’s HSD
two.way <- aov(C~factor(V1) + factor (V2), data = Erhebung)

tukey.two.way<-TukeyHSD(two.way)

tukey.two.way






tukey.plot.aov<-aov(Group ~ factor(V1):factor(V2), data=Erhebung)
tukey.plot.test<-TukeyHSD(tukey.plot.aov)
plot(tukey.plot.test, las = 1)

mean.Group.data <- Erhebung %>%
  group_by(V1, V2) %>%
  summarise(
    Group = mean(Group)
  )

mean.Group.data$group <- c("a","b","b","b","b","c")

mean.Group.data


two.way.plot <- ggplot(Erhebung, aes(x = V1, y = Group, group=V2)) +
  geom_point(cex = 1.5, pch = 1.0,position = position_jitter(w = 0.1, h = 0))

two.way.plot


two.way.plot <- two.way.plot +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2) +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange') +
  geom_point(data=mean.Group.data, aes(x=V1, y=Group))

two.way.plot


two.way.plot <- two.way.plot +
  geom_text(data=mean.Group.data, label=mean.Group.data$group, vjust = -8, size = 5) +
  facet_wrap(~ V1)

two.way.plot


two.way.plot <- two.way.plot +
  theme_classic2() +
  labs(title = "Crop yield in response to fertilizer mix and planting density",
       x = "Planting density (1=low density, 2=high density)",
       y = "Yield (bushels per acre)")

two.way.plot
