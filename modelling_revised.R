install.packages("tidymv")
library(tidyverse)
library(propagate)
library(factoextra)
library(ggfortify)
library(mgcv)
library(gratia)
library(tidygam)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(broom)
library(dplyr)
library(gridExtra)
library(lme4)
library(tidygam)

data<-read.csv("indices_of_biodiversity_final_and_ccf.csv")
View(data)

#filter out abandoned cants
data_act<-data %>%
  filter(age!="abandoned")
View(data_act)
str(data_act)
data_act$age<-as.numeric(data_act$age)

#pca
pca<-prcomp(data_act[, c("shannon_ground", "shannon_shrub",
                     "simpson_ground","simpson_shrub","pielou_even_ground",
                     "pielou_even_shrub","cant_richness","mean_canopy_closure")], 
            center=TRUE, scale.=TRUE, cor=TRUE)
summary(pca)
get_pca_var(pca)
cors<-cor(data_act[, c("shannon_ground", "shannon_shrub",
                       "simpson_ground","simpson_shrub","pielou_even_ground",
                       "pielou_even_shrub","cant_richness","mean_canopy_closure")])
View(cors)
write.csv(cors, "pca_correlations.csv", row.names = TRUE)
pca$sdev
 pca$sdev^2 / sum(pca$sdev^2)
pca$rotation
head(pca$x)

variance <- pca$sdev^2
prop_variance <- variance / sum(variance)

vars<-c("H' ground", "H' shrub",
        "1-D ground","1-D shrub",
        "J' ground","J' shrub",
        "n species","CCF")
rownames(pca$rotation) <-vars

#visualizing pca

p <- autoplot(pca,
              data = data_act,
              colour = "age",
              size=2.5,
              loadings = TRUE,
              loadings.label = FALSE) +
  scale_color_viridis_c(option = "plasma") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  labs(colour = "Cant age") +
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")    # vertical center line
p

arrow_scale <- 0.5
loadings <- as.data.frame(pca$rotation[, 1:2])
loadings$PC1 <- loadings$PC1 * arrow_scale
loadings$PC2 <- loadings$PC2 * arrow_scale
loadings$custom_label <- vars

p + geom_text_repel(data = loadings,
                    aes(x = PC1, y = PC2, label = vars),
                    size = 3.5,
                    color = "black")

# Scree plot
plot(prop_variance, type = "b", xlab = "Principal component", 
     xlim = c(1,5),
     ylab = "Proportion of variance explained",
     ylim = c(0, 1))

#cosine-sqrd to dimensions 1 and 2
fviz_cos2(pca, choice = "var", axes = 1:2)

df$PC1 <- pca_r$x[,1]
df$PC2 <- pca_r$x[,2]
df$cant <- data_act$cant

#age and closure 
data_act$year<-as.factor(data_act$year)
data_act$age_log<-log(data_act$age)
data_23<-data_act %>%
  filter(year==2023)
data_24<-data_act %>%
  filter(year==2024)

gamtest<-gam(mean_closure ~ s(age) + year, data=data_act)
summary(gamtest)
coef(gamtest)
gam.check(gamtest)
plot(gamtest, residuals = TRUE, xlab="Age")
data_act$gam_pred<-predict(gamtest)

closure<-ggplot(data_act, aes(x=age, y=mean_closure))
closure + labs(x="Age (years)", y="Closure", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line()           # Add axis lines back
  ) +
  geom_point(data = data_act, aes(x = age, y = mean_closure, color=year), alpha = 0.5) +
  labs(color = "Survey year") +
  geom_smooth(aes(y = gam_pred), color = "black", linewidth = 1, linetype="dashed")
  theme(axis.title = element_text(size = 18))

#asymptotic models
  
asym_test<- nlsList(mean_closure ~ SSasymp(age, Asym, R0, lrc) | year, 
                       data = data_act)
summary(asym_test)

asym_plot_23<-ggplot(data_23, aes(x=age, y=mean_closure)) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line()           # Add axis lines back
  ) +
  labs(x="Age (Years) in 2023", y = "Canopy Cover") +
  geom_point(data = data_23, aes(x = age, y = mean_closure), color="blue", 
             alpha = 0.5, size=3) +
  geom_line(aes(y = predicted), color = "black", size = 1, linetype="dashed")  +
  theme(axis.title = element_text(size = 7))

asym_model2 <- nls(
  mean_closure ~ Asym - (Asym - R0) * exp(-k * age),
  data = data_24,
  start = list(Asym = 0.873, R0 = 0, k = 0.22),
  lower = c(Asym = -Inf, R0 = 0, k = -Inf),
  upper = c(Asym = 1, R0 = 0, k = Inf),
  algorithm = "port"
)
data_24$predicted <- predict(asym_model2)
data_24$resid<-residuals(asym_model2)

asym_plot_24<-ggplot(data_24, aes(x=age, y=mean_closure)) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line()           # Add axis lines back
  ) +
  ylim(0,1) +
  labs(x="Age (Years) in 2024", y = " ") +
  geom_point(data = data_24, aes(x = age, y = mean_closure, color="year"), color="blue", 
             alpha = 0.5, size=3, shape=17) +
  geom_line(aes(y = predicted), color = "black", size = 1, linetype="dashed") +
  theme(axis.title = element_text(size = 7))

grid.arrange(asym_plot_23, asym_plot_24, ncol=2)

AIC(asym_model)
AIC(asym_model2)

#######break#######

#generalized additive modelling

#MEAN CLOSURE as independent variable in the GAM
#ground biodiversity
#shannon index as dependent variable in the GAM
data_act$year<-as.factor(data_act$year)
data_act$cant<-as.factor(data_act$cant)
gamtest_shan<-gam(shannon_ground ~ s(mean_closure, bs = "cr") + 
                                   s(cant, bs = "re") + 
                                   year, 
                                   data=data_act, method="REML")
gam.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

#plot data and model
data_act$gam_shan_g<-predict(gamtest_shan)
shan_g<-ggplot(data_act, aes(x=mean_closure, y=shannon_ground)) +
  labs(x="Canopy Cover", y="Shannon Index (H') (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none",
    axis.title = element_text(size = 7)
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = shannon_ground, shape=year),
             color="#5EE7F1", size = 3) +
  geom_smooth(aes(y = gam_shan_g), color = "black", linewidth = 1, linetype="dashed")

simp_gam<-gam(simpson_ground ~ s(mean_closure) +
                               s(cant, bs = "re") + 
                               year, data=data_act, method="REML")
gam.check(simp_gam)
k.check(simp_gam)
summary(simp_gam)
plot(simp_gam, select=3, residuals=TRUE)
plot(simp_gam, pages=1, all.terms=TRUE)

#plot data, no model
simp_g<-ggplot(data_act, aes(x=mean_closure, y=simpson_ground)) +
  labs(x="Canopy Cover", y="Inverse Simpson Index (1-D) (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = simpson_ground, shape=year),
             color="#F791A2", size = 3)  +
  theme(axis.title = element_text(size = 7))

#pielou's J as dependent
even_multi<-gam(pielou_even_ground ~ s(mean_closure) +
                                     s(cant, bs = "re") + 
                                     year, data=data_act, method="REML")
gam.check(even_multi)
k.check(even_multi)
summary(even_multi)
plot(even_multi, select=3, residuals=TRUE)
plot(even_multi, pages=1, all.terms=TRUE)

#plot data, no model
even_g<-ggplot(data_act, aes(x=mean_closure, y=pielou_even_ground)) +
 labs(x="Canopy Cover", y="Pielou's Index (J') (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = pielou_even_ground, shape=year),
             color="#F2C463", size = 3)  +
  theme(axis.title = element_text(size = 7))

#richness
richness<-gam(cant_richness ~ s(mean_closure) +
                              s(cant, bs = "re") + 
                              year, data=data_act, method="REML")
gam.check(richness)
k.check(richness)
summary(richness)
plot(richness, select=3, residuals=TRUE)
plot(richness, pages=1, all.terms=TRUE)

#plot data and model (cover sig term)
data_act$rich_cc_pred<-predict(richness)
rich_cc<-ggplot(data_act, aes(x=mean_closure, y=cant_richness)) + 
  labs(x="Canopy Cover", y="Species Richness", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = cant_richness, shape=year),
             color="#C59CF7", size = 3) +
  geom_smooth(aes(y = rich_cc_pred), color = "black", linewidth = 1, linetype="dashed") +
  theme(axis.title = element_text(size = 7))

#shrub layer biodiversity

#shrub shannon as dependent variable in GAM 
gamtest_shan<-gam(shannon_shrub ~ s(mean_closure)+
                    s(cant, bs = "re") + 
                    year, data=data_act, method="REML")
gam.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

#plot data and model
data_act$gam_shan_s<-predict(gamtest_shan)
shan_s<-ggplot(data_act, aes(x=mean_closure, y=shannon_shrub)) +
  labs(x="Canopy Cover", y="Shannon Index (H') (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = shannon_shrub, shape=year),
             color="#5EE7F1", size = 3) +
  theme(axis.title = element_text(size = 7))

#shrub simpson as dependent

simp_gam<-gam(simpson_shrub ~ s(mean_closure) +
                s(cant, bs = "re") + 
                year, data=data_act, method="REML")
gam.check(simp_gam)
k.check(simp_gam)
summary(simp_gam)
plot(simp_gam, select=3, residuals=TRUE)
plot(simp_gam, pages=1, all.terms=TRUE)

#plot data and model
data_act$gam_simp_s<-predict(simp_gam)
simp_s<-ggplot(data_act, aes(x=mean_closure, y=simpson_shrub)) + 
  labs(x="Canopy Cover", y="Inverse Simpson Index (1-D) (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),           
    legend.position="none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = simpson_shrub, shape=year),
             color="#F791A2", size = 3) +
  theme(axis.title = element_text(size = 7))

#pielou's J as dependent variable
even_multi<-gam(pielou_even_shrub ~ s(mean_closure) +
                  s(cant, bs = "re") + 
                  year, data=data_act, method="REML")
gam.check(even_multi)
k.check(even_multi)
summary(even_multi)
plot(even_multi, select=3, residuals=TRUE)
plot(even_multi, pages=1, all.terms=TRUE)

#plot data and model (cover sig term)
data_act$even_cc_s_pred<-predict(even_multi)
even_s_cc<-ggplot(data_act, aes(x=mean_closure, y=pielou_even_shrub)) + 
  labs(x="Canopy Cover", y="Pielou's Index (J') (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),  
    legend.position = "none"
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = pielou_even_shrub, shape=year),
             color="#F2C463", size = 3) +
  labs(shape = "Survey Year") +
  geom_smooth(aes(y = even_cc_s_pred), color = "black", linewidth = 1, 
              linetype="dashed") +
  theme(axis.title = element_text(size = 7))

grid.arrange(shan_g, simp_g, even_g, rich_cc, shan_s, simp_s, even_s_cc, ncol=4)

#AGE as independent variable
#ground
#shannon
shang_age<-gam(shannon_ground ~ s(age, bs = "cr")+
                    s(cant, bs = "re") + 
                    year, data=data_act, method="REML")
gam.check(shang_age)
summary(shang_age)
plot(shang_age, select=3, residuals=TRUE)
plot(shang_age, pages=1, all.terms=TRUE)

#plot data and model (age sig term)
data_act$shang_age_pred<-predict(shang_age)
shan_g_age<-ggplot(data_act, aes(x=age, y=shannon_ground)) + 
  labs(x="Age (Years)", y="Shannon Index (H') (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none",
    axis.title = element_text(size = 7)
  ) +
  geom_point(data = data_act, aes(x = age, y = shannon_ground, shape=year),
             color="#5EE7F1", size = 3) +
  labs(shape = "Survey Year") +
  geom_smooth(aes(y = shang_age_pred), color = "black", linewidth = 1, linetype="dashed")

#simpson as dependent

simpg_age<-gam(simpson_ground ~ s(age)+
                s(cant, bs = "re") + 
                year, data=data_act, method="REML")
gam.check(simpg_age)
k.check(simpg_age)
summary(simpg_age)
plot(simpg_age, select=3, residuals=TRUE)
plot(simpg_age, pages=1, all.terms=TRUE)

#plot data, no model
simp_g_age<-ggplot(data_act, aes(x=age, y=simpson_ground)) +
labs(x="Age (Years)", y="Inverse Simpson Index (1-D) (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"
  ) +
  geom_point(data = data_act, aes(x = age, y = simpson_ground, shape=year),
             color="#F791A2", size = 3) +
  labs(shape = "Survey Year") +
  theme(axis.title = element_text(size = 7))

#pielou's J as dependent
eveng_age<-gam(pielou_even_ground ~ s(age) +
                  s(cant, bs = "re") + 
                  year, data=data_act, method="REML")
gam.check(eveng_age)
k.check(eveng_age)
summary(eveng_age)
plot(eveng_age, select=3, residuals=TRUE)
plot(eveng_age, pages=1, all.terms=TRUE)

even_g_age<-ggplot(data_act, aes(x=age, y=pielou_even_ground)) + 
  labs(x="Age (Years)", y="Pielou's Index (J') (Ground)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"
  ) +
  geom_point(data = data_act, aes(x = age, y = pielou_even_ground, shape=year),
             color="#F2C463", size = 3) +
  labs(shape = "Survey Year") +
  theme(axis.title = element_text(size = 7))

#richness as dependent
r_age<-gam(cant_richness ~ s(age) +
                s(cant, bs = "re") + 
                year, data=data_act, method="REML")
gam.check(r_age)
k.check(r_age)
summary(r_age)
plot(r_age, select=3, residuals=TRUE)
plot(r_age, pages=1, all.terms=TRUE)

#plot data and model (age sig term)
data_act$r_age_pred<-predict(r_age)
r_age_plot<-ggplot(data_act, aes(x=age, y=cant_richness)) + 
  labs(x="Age (Years)", y="Species Richness", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"         
  ) +
  geom_point(data = data_act, aes(x = age, y = cant_richness, shape=year),
             color="#C59CF7", size = 3) +
  labs(shape = "Survey Year") +
  geom_smooth(aes(y = r_age_pred), color = "black", linewidth = 1, linetype="dashed") +
theme(axis.title = element_text(size = 7))

#shrub layer
#shannon
gamtest_shan<-gam(shannon_shrub ~ s(age)+
                    s(cant, bs = "re") + 
                    year, data=data_act, method="REML")
gam.check(gamtest_shan)
k.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)
data_act$shan_s_pred<-predict(gamtest_shan)

#plot data with model (cant id sig term)
shan_s_age<-ggplot(data_act, aes(x=age, y=shannon_shrub)) +
  labs(x="Age (Years)", y="Shannon Index (H') (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"           
  ) +
  geom_point(data = data_act, aes(x = age, y = shannon_shrub, shape=year),
             color="#5EE7F1", size = 3) +
  labs(shape = "Survey Year") +
  geom_smooth(aes(y = shan_s_pred), color = "black", linewidth = 1, linetype="dashed") +
  theme(axis.title = element_text(size = 7))

#simpson
simps_age<-gam(simpson_shrub ~ s(age) +
                s(cant, bs = "re") + 
                year, data=data_act, method="REML")
summary(simps_age)
gam.check(simps_age) 
k.check(simps_age)
plot(simps_age, select=3, residuals=TRUE)
plot(simps_age, pages=1, all.terms=TRUE)

#plot data with model (cant id sig term)
data_act$simp_s_pred<-predict(simps_age)
simp_s_age<-ggplot(data_act, aes(x=age, y=simpson_shrub)) + 
  labs(x="Age (Years)", y="Inverse Simpson Index (1-D) (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"
    ) +
  geom_point(data = data_act, aes(x = age, y = simpson_shrub, shape=year),
             color="#F791A2", size = 3) +
  labs(shape = "Survey Year") +
  geom_smooth(aes(y = simp_s_pred), color = "black", linewidth = 1, linetype="dashed") +
  theme(axis.title = element_text(size = 7))

#pielou's J
evens_age<-gam(pielou_even_shrub ~ s(age) +
                  s(cant, bs = "re") + 
                  year, data=data_act, method="REML")
gam.check(evens_age) 
k.check(evens_age)  
summary(evens_age)
plot(evens_age, select=3, residuals=TRUE)
plot(evens_age, pages=1, all.terms=TRUE)

#plot data, no model
even_s_age<-ggplot(data_act, aes(x=age, y=pielou_even_shrub)) + 
  labs(x="Age (Years)", y="Pielou's Index (J') (Shrub)", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line(),          
    legend.position = "none"
  ) +
  geom_point(data = data_act, aes(x = age, y = pielou_even_shrub, shape=year),
             color="#F2C463", size = 3) +
  labs(shape = "Survey Year") +
  theme(axis.title = element_text(size = 7))

grid.arrange(shan_g_age, simp_g_age, even_g_age, r_age_plot, shan_s_age, simp_s_age, even_s_age, ncol=4)