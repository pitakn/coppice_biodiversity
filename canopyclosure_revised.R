setwd("~/Chapter 2")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
library(vegan)

canopy<-read.csv("canopyclosure.csv")
View(canopy)
canopy$fraction <- as.numeric(canopy$fraction)
canopy$year <-as.character(canopy$year)

#subset without null values and remove white pixels
canopy <- canopy %>%
  filter(fraction != "NULL")
canopy <- canopy %>%
  filter(value != "255")
View(canopy)

#subset data by cant to evaluate differences between years
caa <- canopy[canopy$plot == "1caa", ]
year_2023 <- caa[caa$year == "2023",]
year_2024 <- caa[caa$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
qqnorm(year_2023$fraction)
qqline(year_2023$fraction, col="green")
qqnorm(year_2024$fraction)
qqline(year_2024$fraction, col="green")

cab <- canopy[canopy$plot == "1cab", ]
year_2023 <- cab[cab$year == "2023",]
year_2024 <- cab[cab$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
qqnorm(year_2023$fraction)
qqline(year_2023$fraction, col="green")
qqnorm(year_2024$fraction)
qqline(year_2024$fraction, col="green")

f <- canopy[canopy$plot == "1f", ]
year_2023 <- f[f$year == "2023",]
year_2024 <- f[f$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

ga <- canopy[canopy$plot == "1ga", ]
year_2023 <- ga[ga$year == "2023",]
year_2024 <- ga[ga$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

gb <- canopy[canopy$plot == "1gb", ]
gb_2023 <- gb[gb$year == "2023",]
gb_2024 <- gb[gb$year == "2024",]
View(gb_2024)

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

gc <- canopy[canopy$plot == "1gc", ]
gc_2023 <- gc[gc$year == "2023",]
gc_2024 <- gc[gc$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

h <- canopy[canopy$plot == "1h", ]
year_2023 <- h[h$year == "2023",]
year_2024 <- h[h$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

i <- canopy[canopy$plot == "1i", ]
year_2023 <- i[i$year == "2023",]
year_2024 <- i[i$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

j <- canopy[canopy$plot == "1j", ]
year_2023 <- j[j$year == "2023",]
year_2024 <- j[j$year == "2024",]

ma <- canopy[canopy$plot == "1ma", ]
ma_2023 <- ma[ma$year == "2023",]
ma_2024 <- ma[ma$year == "2024",]

mb <- canopy[canopy$plot == "1mb", ]
mb_2023 <- mb[mb$year == "2023",]
mb_2024 <- mb[mb$year == "2024",]

mc <- canopy[canopy$plot == "1mc", ]
mc_2023 <- mc[mc$year == "2023",]
mc_2024 <- mc[mc$year == "2024",]

n <- canopy[canopy$plot == "1n", ]
year_2023 <- n[n$year == "2023",]
year_2024 <- n[n$year == "2024",]

o <- canopy[canopy$plot == "1o", ]
o_2023 <- o[o$year == "2023",]
o_2024 <- o[o$year == "2024",]
View(o_2024)

e <- canopy[canopy$plot == "2e", ]
year_2023 <- e[e$year == "2023",]
year_2024 <- e[e$year == "2024",]

#Wilcoxon tests between CWS and non CWS cants with same ages
#c vs B (age 3) and C vs D (age 4)
wilcox.test(gc_2023$fraction, o_2024$fraction)
#result: not significantly different - p = 0.2414
wilcox.test(gc_2024$fraction, gb_2023$fraction)
#result: significantly different - p = 0.000999; which direction?
par(mfrow = c(1,2))
boxplot(gc_2024$fraction, ylim = range(c(0,1), na.rm = TRUE), ylab="Canopy Cover", xlab = "Cant C 2024 (Age 4; CWS)")
boxplot(gb_2023$fraction, ylim = range(c(0,1), na.rm = TRUE), xlab="Cant D 2023 (Age 4; SC)")
#gc (C) has LOWER COVER at age 4 with CWS than gb (D) at age 4
#H vs G (age 11)
wilcox.test(ma_2023$fraction, mb_2024$fraction)
#result: significantly different - p = 0.014; which direction?
par(mfrow = c(1,2))
boxplot(ma_2023$fraction, ylim = range(c(0,1), na.rm = TRUE), ylab="Canopy Cover", xlab = "Cant H 2023 (Age 11; CWS)")
boxplot(mb_2024$fraction, ylim = range(c(0,1), na.rm = TRUE), xlab="Cant G 2024 (Age 11; SC)")
#result: slightly higher in CWS system here - compare same year for two cants?
wilcox.test(ma_2023$fraction, mb_2023$fraction)
#result: not significantly different between each other in 2023 - p = 0.89
#difference is much more likely due to year effect than CWS
#mb decreased in cc from 2023-2024 (as did ma)
par(mfrow = c(1,2))
boxplot(ma_2023$fraction, ylim = range(c(0,1), na.rm = TRUE), ylab="Canopy Cover", xlab = "Cant H 2023 (Age 11; CWS)")
boxplot(mb_2023$fraction, ylim = range(c(0,1), na.rm = TRUE), xlab="Cant G 2023 (Age 10; SC)")

#Wilcoxon test between years
test <- wilcox.test(year_2023$fraction, year_2024$fraction)
print (test)

boxplot(year_2023$fraction, ylim = range(c(0,1), na.rm = TRUE), ylab="Canopy Closure Fraction")
boxplot(year_2024$fraction, ylim = range(c(0,1), na.rm = TRUE), ylab="Canopy Closure Fraction")

#remove abandoned cants
canopy <- canopy %>%
  filter(plot != "1n",
         plot != "1mc",
         plot != "1ga",
         plot != "1f",
         plot != "1h")
View(canopy)

#calculate mean canopy closure per cant each year
canopy$fraction_paste <- as.numeric(canopy$fraction_paste)
canopy$year <-as.character(canopy$year)
str(canopy)
closure <- canopy %>%
  group_by(plot,year) %>%
  summarise(mean_closure = mean(fraction, na.rm = TRUE), 
            se = sd(fraction) / sqrt(n()),
            .groups = 'drop')
View(closure)

#add ages
closure<- closure %>%
  mutate(age=recode(plot, "1caa"="0", "1cab"="14", "1f"="30", 
                    "1ga"="30", "1gb"="4","1gc"="3","1h"="30","1i"="6","1j"="21","1ma"="11","1mb"="10","1mc"="30","1n"="30","1o"="2","2e"="8")
  )
closure$age<-as.numeric(closure$age)
closure$age <- ifelse(closure$year == 2024, closure$age + 1, closure$age)

write.csv(closure, "closure.csv", row.names = FALSE)

#active cants
closure_active<-closure %>%
  filter(age <=25)
View(closure_active)
closure_active <- closure_active[order(closure_active$age), ]

ggplot(closure_active, aes(x = age, y = mean_closure, colour = factor(year))) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 1)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_closure - se, ymax = mean_closure + se),
    width = 0.2,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant age", y = "Mean canopy closure fraction", colour="Survey Year", ) +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")
        ) +
  geom_line(data = data_act, aes(y = predicted), color = "black", size = 1, linetype="dashed")

#abandoned cants

closure_aban <- closure %>%
  filter(plot %in% c("1f","1ga","1h","1mc","1n"))
View(closure_aban)
closure_aban<- closure_aban %>%
  mutate(plot=recode(plot, "1f"="K(a)", 
                    "1ga"="L(a)", "1h"="M(a)","1mc"="N(a)","1n"="P(a)")
  )

ggplot(closure_aban, aes(x = plot, y = mean_closure, colour = factor(year))) +
  geom_point(size=3) +
  scale_y_continuous(limits = c(0, 1)) + #plot for means
  geom_errorbar(
    aes(ymin = mean_closure - se, ymax = mean_closure + se),
    width = 0.05,  # Error bar width
    color = "black"
  ) +
  labs(x = "Cant ID", y = "Mean canopy closure fraction", colour="Survey Year") +
  theme_minimal() +
  theme(legend.position = "right",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")
        )
