setwd("~/Chapter 2")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
library(vegan)

biodiv2024<-read.csv("2024_bd_clean.csv")
View(biodiv2024)

#generate species richness numbers
unique_counts_24 <-biodiv2024 %>%
  group_by(plotid) %>%  # Group the data by category_column
  summarise(unique_count = n_distinct(plantid))
View(unique_counts_24)

#filter by vegetation layer
#GROUND
biodiv_gr2024<-biodiv2024 %>%
  filter(layer != "shrub")
View(biodiv_gr2024)

#total cover over all 4 quadrats in each plot
biodiv_g_sum2<- biodiv_gr2024 %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = sum(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_g_sum2)

#total of unique species across all cants
plot_species2 <- biodiv_g_sum2 %>%
  group_by(plantid) %>%
  summarise(total_cover = mean(total_cover, na.rm = TRUE), .groups = 'drop')
View(plot_species2)

#generate frequency table for biodiversity indices
frequency_table2024<-biodiv_g_sum2 %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = total_cover/4, .groups='drop') %>%
  complete(plotid, plantid=plot_species2$plantid, fill=list(total_cover=0))
View(frequency_table2024)
cover<-as.numeric(frequency_table2024$total_cover)
frequency_table2024 <- frequency_table2024 %>%
  pivot_wider(names_from = plantid, values_from = total_cover)
frequency_table2024<-frequency_table2024[, -1]
rownames(frequency_table2024) <- c("1caa","1cab","1f","1ga","1gb","1gc","1h","1i","1j","1ma","1mb",
                               "1mc","1n","1o","2e")
str(frequency_table2024)

#biodiversityindices
shannon_diversity_24 <- diversity(frequency_table2024, index = "shannon")
simpson_diversity_24 <- diversity(frequency_table2024, index = "simpson")

#evenness
num_species_24 <- apply(frequency_table2024, 1, function(x) sum(x > 0))
shannon_evenness_24 <- shannon_diversity_24 / log(num_species_24)
J_24 <- shannon_diversity_24/log(specnumber(frequency_table2024))
print(J_24)

diversity_results_g_24 <- data.frame(
  shannon = shannon_diversity_24,
  simpson = simpson_diversity_24,
  richness = unique_counts_24,
  evenness = J_24
)
View(diversity_results_g_24)
write.csv(diversity_results_g_24, file = "divresults_g_24.csv", row.names = TRUE)

#SHRUB
#filter to shrub layer
biodiv_s2024<-biodiv2024 %>%
  filter(layer != "ground")
View(biodiv_s2024)

#total cover over all 4 quadrats in each plot
biodiv_s_sum2<- biodiv_s2024 %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = sum(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_s_sum2)

#total of unique species across all cants and average of their cover across all cants
plot_species_s <- biodiv_s_sum2 %>%
  group_by(plantid) %>%
  summarise(total_cover = mean(total_cover, na.rm = TRUE), .groups = 'drop')
View(plot_species_s)

#generate frequency table for biodiversity indices
frequency_table_s_2024<-biodiv_s_sum2 %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = total_cover/4, .groups='drop') %>%
  complete(plotid, plantid=plot_species_s$plantid, fill=list(total_percent_cover=0))
View(frequency_table_s_2024)

cover<-as.numeric(frequency_table_s_2024$total_cover)
frequency_table_s_2024 <- frequency_table_s_2024 %>%
  pivot_wider(names_from = plantid, values_from = total_cover)
frequency_table_s_2024<-frequency_table_s_2024[, -1]
rownames(frequency_table_s_2024) <- c("1caa","1cab","1f","1ga","1gb","1gc","1h","1i","1j","1ma","1mb",
                                   "1mc","1n","1o","2e")
str(frequency_table_s_2024)

frequency_table_s_2024[is.na(frequency_table_s_2024)] <- 0

#biodiversityindices
shannon_diversity_s24 <- diversity(frequency_table_s_2024, index = "shannon")
simpson_diversity_s24 <- diversity(frequency_table_s_2024, index = "simpson")

#evenness
num_species_s24 <- apply(frequency_table_s_2024, 1, function(x) sum(x > 0))
shannon_evenness_s24 <- shannon_diversity_s24 / log(num_species_s24)
J_s24 <- shannon_diversity_s24/log(specnumber(frequency_table_s_2024))

diversity_results_s_24 <- data.frame(
  shannon = shannon_diversity_s24,
  simpson = simpson_diversity_s24,
  richness = num_species_s24,
  evenness = J_s24
)
View(diversity_results_s_24)
write.csv(diversity_results_s_24, file = "divresults_s_24.csv", row.names = TRUE)

#boxplots for species richness
highforest<-read.csv("highforestversuscoppice.csv")
View(highforest)
Active <- highforest$richness[highforest$coppice_highforest == "coppiced"]
Abandoned <- highforest$richness[highforest$coppice_highforest == "highforest"]
par(mfrow = c(1, 2))

highforest_24<-read.csv("highforestversuscoppice_2024.csv")
View(highforest_24)
Active_24 <- highforest_24$richness[highforest_24$abandoned == "n"]
Abandoned_24 <- highforest_24$richness[highforest_24$abandoned == "y"]
par(mfrow = c(1, 4))
boxplot(Active, ylim = range(highforest$richness), col=c("lightblue"),
        ylab = "Species richness (n)", xlab = "Actively Managed 2023")
boxplot(Abandoned, xlab = "Abandoned 2023", ylim = range(highforest$richness), col=c("lightgreen"))
boxplot(Active_24, ylim = range(highforest_24$richness), col=c("#23a7c2"),
        xlab = "Actively Managed 2024")
boxplot(Abandoned_24, xlab = "Abandoned 2024", ylim = range(highforest_24$richness), col=c("#13ad4c"))

abandoned_info<-c("coppiced", "coppiced", "highforest", "highforest","coppiced", "coppiced",
                  "highforest", "coppiced", "highforest", "coppiced", "coppiced", "highforest",
                  "highforest","coppiced", "coppiced")

highforest_24<-cbind(abandoned_info, highforest_24)