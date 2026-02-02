setwd("~/Chapter 2")
library(dplyr)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(packcircles)
library(ggforce)
install.packages('vegan')
library(vegan)

#import data
biodiv_final<-read.csv("biodiv_final_2023.csv")
View(biodiv_final)

#finish cleaning the data
biodiv_final<-biodiv_final %>%
  mutate(plantid=recode(plantid, "DRYO"="DRYODIL", "BETU"="BETUSPP", "RUMEOBS"="RUMEOBT", 
                        "CIRCSPP"="CIRSSPP"))
biodiv_final<-biodiv_final %>%
  filter(!plotid %in% c("5a1", "5a2"))
View(biodiv_final)

#GROUND LAYER
#filter by vegetation layer
biodiv_ground<-biodiv_final %>%
  filter(layer != "shrub")
View(biodiv_ground)

#age descriptive statistics
biodiv_ground$plot_age<-as.numeric(biodiv_ground$plot_age)
biodiv_ground<-biodiv_ground %>%
  mutate(plot_age=if_else(plot_age==21, 20, plot_age))
summary(biodiv_ground)

#generate species richness numbers
unique_counts <-biodiv_ground %>%
  group_by(plotid) %>%  # Group the data by category_column
  summarise(unique_count = n_distinct(plantid))
View(unique_counts)

#total area cover over all 4 quadrats in each plot
biodiv_g_sum<- biodiv_ground %>%
  group_by(plotid,plantid) %>%
  summarise(total_percent_cover = sum(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_g_sum)

#calculate number of plots each species showed up in
plotplants23<-biodiv_g_sum %>%
  group_by(plantid) %>%
  summarise(record_count = n())
View(plotplants23)

#generate frequency table
frequency_tableg<-biodiv_g_sum %>%
  group_by(plotid,plantid) %>%
  summarise(total_percent_cover = total_percent_cover/4, .groups='drop') %>%
  complete(plotid, plantid=biodiv_g_sum$plantid, fill=list(total_percent_cover=0))
View(frequency_tableg)

#pivotwider
frequency_table_w <- frequency_tableg %>%
  pivot_wider(names_from = plantid, values_from = total_percent_cover)
frequency_table_w<-frequency_table_w[, -1]
rownames(frequency_table_w) <- c("1caa","1cab","1f","1ga","1gb","1gc","1h","1i","1j","1ma","1mb",
                                 "1mc","1n","1o","2e")
View(frequency_table_w)
str(frequency_table_w)

#SHRUBS
#filter by vegetation layer to shrubs
biodiv_shrub<-biodiv_final %>%
  filter(layer != "ground")
View(biodiv_shrub)

#generate species richness numbers
unique_counts_s <-biodiv_shrub %>%
  group_by(plotid) %>%  # Group the data by category_column
  summarise(unique_count = n_distinct(plantid))
View(unique_counts_s)

#total area cover over all 4 quadrats in each plot
biodiv_s_sum<- biodiv_shrub %>%
  group_by(plotid,plantid) %>%
  summarise(total_percent_cover = sum(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_s_sum)

#calculate number of plots each species showed up in
plotplants23<-biodiv_s_sum %>%
  group_by(plantid) %>%
  summarise(record_count = n())
View(plotplants23)

#generate shrub frequency table
frequency_table_s<-biodiv_s_sum %>%
  group_by(plotid,plantid) %>%
  summarise(total_percent_cover = total_percent_cover/4, .groups='drop') %>%
  complete(plotid, plantid=biodiv_s_sum$plantid, fill=list(total_percent_cover=0))
View(frequency_table_s)

#pivot wider shrub frequency table
frequency_table_s_w <- frequency_table_s %>%
  pivot_wider(names_from = plantid, values_from = total_percent_cover)
frequency_table_s_w<-frequency_table_s_w[, -1]
rownames(frequency_table_s_w) <- c("1caa","1cab","1f"," 1ga","1gb","1gc","1h","1i","1j","1ma","1mb",
                               "1mc","1n","1o","2e")
View(frequency_table_s_w)

#average percent cover across all plots
plot_species1 <- frequency_table_s %>%
  group_by(plantid) %>%
  summarise(total_percent_cover = mean(total_percent_cover, na.rm = FALSE))
View(plot_species1)

#total area cover across plots with plant presence
plot_species2 <- biodiv_s_sum %>%
  group_by(plantid) %>%
  summarise(total_percent_cover = mean(total_percent_cover, na.rm = FALSE))
View(plot_species2)

#biodiversityindices

#ground biodiversity
shannon_diversity <- diversity(frequency_table_w, index = "shannon")
View(shannon_diversity)
simpson_diversity <- diversity(frequency_table_w, index = "simpson")
print(simpson_diversity)

#ground evenness
num_species <- apply(frequency_table_w, 1, function(x) sum(x > 0))
shannon_evenness <- shannon_diversity / log(num_species)
print(shannon_evenness)
J <- shannon_diversity/log(specnumber(frequency_table_w))
print(J)

diversity_results_g <- data.frame(
  shannon = shannon_diversity,
  simpson = simpson_diversity,
  richness = unique_counts,
  evenness = J
)
View(diversity_results_g)
write.csv(diversity_results_g, file = "divresults_g.csv", row.names = TRUE)

#shrub biodiversity
shannon_diversity_s <- diversity(frequency_table_s_w, index = "shannon")
print(shannon_diversity_s)
simpson_diversity_s <- diversity(frequency_table_s_w, index = "simpson")
View(invsimpson_diversity_s)

#shrub evenness
num_species_s <- apply(frequency_table_s_w, 1, function(x) sum(x > 0))
shannon_evenness_s <- shannon_diversity_s / log(num_species_s)
print(shannon_evenness_s)
J_s <- shannon_diversity_s/log(specnumber(frequency_table_s_w))
print(J_s)

diversity_results_s <- data.frame(
  shannon = shannon_diversity_s,
  simpson = simpson_diversity_s,
  richness = unique_counts_s,
  evenness = J_s
)
View(diversity_results_s)
write.csv(diversity_results_s, file = "divresults_s.csv", row.names = TRUE)

#sorensen
#GROUND
sorensen_dist <- vegdist(frequency_table_w, method = "bray")
# Convert to similarity by subtracting from 1
sorensen_similarity <- 1 - as.matrix(sorensen_dist)
community_pca<-prcomp(sorensen_similarity)

c <- autoplot(community_pca,
              data = sorensen_similarity,
              size=2.5,
              loadings = TRUE,
              loadings.label = TRUE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")    # vertical center line
plot(c)

View(sorensen_similarity)
write.csv(sorensen_similarity, file = "bray_23g.csv", row.names = TRUE)

#SHRUB
sorensen_dist <- vegdist(frequency_table_s_w, method = "bray")
# Convert to similarity by subtracting from 1
sorensen_similarity <- 1 - as.matrix(sorensen_dist)
View(sorensen_similarity)
write.csv(sorensen_similarity, file = "bray_23s.csv", row.names = TRUE)

# View results
print(sorensen_similarity)circles <- circleProgressiveLayout(plot_species$mean_percent_cover)


#OTHER DESCRIPTIVE STATS
#GROUND

#average percent cover across all plots
plot_species1 <- frequency_table %>%
  group_by(plantid) %>%
  summarise(total_percent_cover = mean(total_percent_cover, na.rm = FALSE))
View(plot_species1)

#average area cover across plots with plant presence
plot_species2 <- biodiv_g_sum %>%
  group_by(plantid) %>%
  summarise(total_percent_cover = mean(total_percent_cover, na.rm = FALSE))
View(plot_species2)

#calculate frequencies of the number of plots each species showed up in
frequency_table<-biodiv_g_mean %>%
  group_by(plantid) %>%
  summarise(record_count = n())
View(frequency_table)

#SHRUB
#filter to shrub layer
biodiv_s<-biodiv_final %>%
  filter(layer != "ground")
View(biodiv_s)

#total percent cover over all 4 quadrats in each plot
biodiv_s_sum<- biodiv_s %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = sum(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_s_sum)

#generate frequency table 
frequency_table_s23<-biodiv_s_sum %>%
  group_by(plotid,plantid) %>%
  summarise(total_cover = total_cover/4, .groups='drop') %>%
  complete(plotid, plantid=plot_species$plantid, fill=list(total_cover=0))
View(frequency_table_s23)

#average percent cover across all plots
plot_species_percent <- frequency_table_s23 %>%
  group_by(plantid) %>%
  summarise(total_cover = mean(total_cover, na.rm = FALSE))
View(plot_species_percent)

#total area cover across plots with plant presence
plot_species_area <- biodiv_s_sum %>%
  group_by(plantid) %>%
  summarise(total_cover = mean(total_cover, na.rm = FALSE))
View(plot_species_area)

#filter to shrub layer
biodiv_shrub<-biodiv_final %>%
  filter(layer != "ground")

#ACTIVE VS ABANDONED

#separate species richness values for abandoned versus active management
highforest<-read.csv("highforestversuscoppice.csv")
View(highforest)
Active <- highforest$richness[highforest$coppice_highforest == "coppiced"]
mean(Active)
Abandoned <- highforest$richness[highforest$coppice_highforest == "highforest"]
mean(Abandoned)
par(mfrow = c(1, 2))
boxplot(Active, main = "Actively Managed", ylim = range(highforest$richness), col=c("lightblue"), 
        ylab="Species richness (n)")
boxplot(Abandoned, main = "Abandoned", ylim = range(highforest$richness), col=c("lightgreen", 
        ylab="Species richness (n)"))

#separating mean percent cover by ACTIVE VS ABANDONED and by layer

#active ground
biodiv_coppice_g<-biodiv_final %>%
  filter(abandoned != "Y", layer !="shrub")
View(biodiv_coppice_g)
biodiv_cg_mean<- biodiv_coppice_g %>%
  group_by(plotid,plantid) %>%
  summarise(mean_percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_cg_mean)

#active shrub
biodiv_coppice_s<-biodiv_final %>%
  filter(abandoned != "Y", layer !="ground")
View(biodiv_coppice_s)
biodiv_cs_mean<- biodiv_coppice_s %>%
  group_by(plotid,plantid) %>%
  summarise(mean_percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_cs_mean)

#abandoned ground
biodiv_aban_g<-biodiv_final %>%
  filter(abandoned != "N", layer !="shrub")
View(biodiv_aban_g)
biodiv_ag_mean<- biodiv_aban_g %>%
  group_by(plotid,plantid) %>%
  summarise(mean_percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_ag_mean)

#abandoned shrub
biodiv_aban_s<-biodiv_final %>%
  filter(abandoned != "N", layer !="ground")
View(biodiv_aban_s)
biodiv_as_mean<- biodiv_aban_s %>%
  group_by(plotid,plantid) %>%
  summarise(mean_percent_cover = mean(percent_cover, na.rm = TRUE), .groups = 'drop')
View(biodiv_as_mean)

#establish list of all species in all plots
unique_species<-unique(biodiv_final$plantid)
print(unique_species)