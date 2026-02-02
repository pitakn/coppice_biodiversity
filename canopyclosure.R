setwd("~/Chapter 2")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
library(vegan)
totalbd<-read.csv("indices_of_biodiversity.csv")
View(totalbd)
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
year_2023 <- gb[gb$year == "2023",]
year_2024 <- gb[gb$year == "2024",]

hist(year_2023$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")
hist(year_2024$fraction, main="", xlab="Bins", ylab="Canopy Closure Fraction")

gc <- canopy[canopy$plot == "1gc", ]
year_2023 <- gc[gc$year == "2023",]
year_2024 <- gc[gc$year == "2024",]

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
year_2023 <- ma[ma$year == "2023",]
year_2024 <- ma[ma$year == "2024",]

mb <- canopy[canopy$plot == "1mb", ]
year_2023 <- mb[mb$year == "2023",]
year_2024 <- mb[mb$year == "2024",]

mc <- canopy[canopy$plot == "1mc", ]
year_2023 <- mc[mc$year == "2023",]
year_2024 <- mc[mc$year == "2024",]

n <- canopy[canopy$plot == "1n", ]
year_2023 <- n[n$year == "2023",]
year_2024 <- n[n$year == "2024",]

o <- canopy[canopy$plot == "1o", ]
year_2023 <- o[o$year == "2023",]
year_2024 <- o[o$year == "2024",]

e <- canopy[canopy$plot == "2e", ]
year_2023 <- e[e$year == "2023",]
year_2024 <- e[e$year == "2024",]

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
