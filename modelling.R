library(propagate)
library(factoextra)
library(ggfortify)
library(mgcv)
library(gratia)
library(tidymv)
library(ggplot2)
library(cowplot)
library(ggrepel)
library(broom)
library(dplyr)
library(gridExtra)

data<-read.csv("indices_of_biodiversity_and_ccf_final.csv")
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
            center=TRUE, scale.=TRUE)
summary(pca)
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

#age and canopy closure
gamtest<-gam(mean_height ~ s(age), data=data_act)
summary(gamtest)
View(gamtest)
coef(gamtest)
gam.check(gamtest)
plot(gamtest, residuals = TRUE, xlab="Age")
plot(gamtest, xlab="Age")

#age and closure asymptotic model
asym_model <- nls(
  mean_closure ~ Asym * exp(-k * age),
  data = data_act,
  start = list(Asym = 1, k=0.26)
)

asym_model <- nls(
  mean_closure ~ Asym - (Asym - R0) * exp(-k * age),
  data = data_act,
  start = list(Asym = 1, R0 = 0, k = 0.26),
  lower = c(Asym = -Inf, R0 = 0, k = -Inf),
  upper = c(Asym = 1, R0 = 0, k = Inf),
  algorithm = "port"
)

summary(asym_model)
AIC(asym_model)
tidy(asym_model)

eq_text <- sprintf(
  "y = %.2f - (%.2f - %.2f) * exp(-%.2f * x)",
  coefs["Asym"], coefs["Asym"], coefs["R0"], coefs["k"]
)

eq_text <- sprintf(
  "y = %.2f * (1 - exp(-%.2f * x))",
  coefs["Asym"], coefs["k"]
)
cat(eq_text)

#generate plot modelling closure with age
data_act$predicted <- predict(asym_model)
data_act$year<-as.factor(data_act$year)

ggplot(data_act, aes(x=age, y=mean_closure)) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line()           # Add axis lines back
  ) +
  labs(x="Cant age (years)", y= "Canopy closure fraction (CCF)") +
  geom_point(data = data_act, aes(x = age, y = mean_closure, color=year), alpha = 0.5, size=2) +
  labs(color = "Survey year") +
  geom_line(aes(y = predicted), color = "black", size = 1, linetype="dashed")

#######break#######

#generalized additive modelling

#MEAN CLOSURE as independent variable in the GAM
#ground biodiversity

#shannon index as dependent variable in the GAM
gamtest_shan<-gam(shannon_ground ~ s(mean_closure), data=data_act, method="REML")
gam.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

shan<-plot_smooths(model=gamtest_shan, series=mean_closure)
shan + labs(x="Closed canopy fraction (CCF)", y="Shannon Index (H')", title=NULL) + 
  theme(
  panel.background = element_blank(),  
  plot.background = element_blank(),   
  panel.grid = element_blank(),       
  axis.line = element_line()           
) +
  geom_point(data = data_act, aes(x = mean_closure, y = shannon_ground), color = "blue", alpha = 0.5)

#shrub simpson as dependent

simp_gam<-gam(simpson_ground ~ s(mean_closure), data=data_act, method="REML")
gam.check(simp_gam)
k.check(simp_gam)
summary(simp_gam)

simp<-plot_smooths(model=simp_gam, series=mean_closure)
simp_nice<-simp + labs(x="Canopy Closure Fraction (CCF)", y="Simpson Index (1-D)", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),       
    axis.line = element_line()         
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = simpson_ground), color = "blue", alpha = 0.5)

#pielou's J as dependent
even_multi<-gam(pielou_even_ground ~ s(mean_closure), data=data_act, method="REML")
gam.check(even_multi)
k.check(even_multi)
summary(even_multi)

even_gam<-plot_smooths(model=even_multi, series=mean_closure)
even_gam + labs(x="Canopy Closure Fraction (CCF)", y="Pielou's evenness (J')", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = pielou_even_ground), color = "blue", alpha = 0.5)

richness<-gam(cant_richness ~s(mean_closure), data=data_act, method="REML")
gam.check(richness)
k.check(richness)
summary(richness)

rich_gam<-plot_smooths(model=richness, series=mean_closure)
rich<-rich_gam + labs(x="Canopy Closure Fraction (CCF)", y="Species richness (n)", title=NULL) + 
  theme(
    panel.background = element_blank(), 
    plot.background = element_blank (),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = cant_richness), color = "blue", alpha = 0.5)

grid.arrange(simp_nice, rich, nrow=1)

#richness x simpson ground interactions

rich_simp<-gam(simpson_ground*cant_richness ~ s(mean_closure), data=data_act, method="REML")
gam.check(rich_simp)
k.check(rich_simp)
summary(rich_simp)


#shrub layer biodiversity
shrub<-read.csv("indices_s.csv")
View(shrub)

#shrub shannon as dependent variable in GAM 
gamtest_shan<-gam(shannon_shrub ~ s(mean_closure), data=data_act, method="REML")
gam.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

shan<-plot_smooths(model=gamtest_shan, series=mean_closure)
shan + labs(x="Closed canopy fraction (CCF)", y="Shannon Index (H')", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = shannon_shrub), color = "blue", alpha = 0.5)

#shrub simpson as dependent

simp_gam<-gam(simpson_shrub ~ s(mean_closure), data=data_act, method="REML")
gam.check(simp_gam)
k.check(simp_gam)
summary(simp_gam)

simp<-plot_smooths(model=simp_gam, series=mean_closure)
simp + labs(x="Canopy Closure Fraction (CCF)", y="Simpson Index", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = simpson_shrub), color = "blue", alpha = 0.5)

#pielou's J as dependent variable
even_multi<-gam(pielou_even_shrub ~ s(mean_closure), data=data_act, method="REML")
gam.check(even_multi)
k.check(even_multi)
summary(even_multi)

even_gam<-plot_smooths(model=even_multi, series=mean_closure)
even_gam + labs(x="Canopy Closure Fraction (CCF)", y="Pielou's evenness (J')", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),       
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = mean_closure, y = pielou_even_shrub), color = "blue", alpha = 0.5)



#AGE as independent variable
#ground
#shannon
gamtest_shan<-gam(shannon_ground ~ s(age), data=data_act, method="REML")
gam.check(gamtest_shan)
 summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

shan<-plot_smooths(model=gamtest_shan, series=age)
shan_plot<-shan + labs(x="Cant age (years)", y="Shannon Index (H')", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = age, y = shannon_ground), color = "blue", alpha = 0.5) +
  theme(axis.title = element_text(size = 16))

#simpson as dependent

simp_gam<-gam(simpson_ground ~ s(age), data=data_act, method="REML")
gam.check(simp_gam)
k.check(simp_gam)
summary(simp_gam)

simp<-plot_smooths(model=simp_gam, series=age)
simp_plot<-simp + labs(x="Cant age (years)", y="Simpson Index (1/D)", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = age, y = simpson_ground), color = "blue", alpha = 0.5) +
  theme(axis.title = element_text(size = 16))
simp_plot

#pielou's J as dependent
even_multi<-gam(pielou_even_ground ~ s(age), data=data_act, method="REML")
gam.check(even_multi)
k.check(even_multi)
summary(even_multi)

even_gam<-plot_smooths(model=even_multi, series=age)
evenplot<-even_gam + labs(x="Cant age (years)", y="Pielou's evenness (J')", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank(),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = age, y = pielou_even_ground), color = "blue", alpha = 0.5) +
  theme(axis.title = element_text(size = 16))

#richness as dependent
richness<-gam(cant_richness ~s(age), data=data_act, method="REML")
gam.check(richness)
k.check(richness)
summary(richness)

rich_gam<-plot_smooths(model=richness, series=age)
rich_plot<-rich_gam + labs(x="Cant age (years)", y="Species richness (n)", title=NULL) + 
  theme(
    panel.background = element_blank(),  
    plot.background = element_blank (),   
    panel.grid = element_blank(),        
    axis.line = element_line()           
  ) +
  geom_point(data = data_act, aes(x = age, y = cant_richness), color = "blue", alpha = 0.5) +
  theme(axis.title = element_text(size = 16))

grid.arrange(shan_plot,simp_plot, evenplot, rich_plot, ncol=2)

#shrub layer
#shannon
gamtest_shan<-gam(shannon_shrub ~ s(age), data=data_act, method="REML")
gam.check(gamtest_shan)
k.check(gamtest_shan)
summary(gamtest_shan)
plot(gamtest_shan, select=3, residuals=TRUE)
plot(gamtest_shan, pages=1, all.terms=TRUE)

shan<-plot_smooths(model=gamtest_shan, series=age)
shan + labs(x="Age (years)", y="Shannon Index (H')", title=NULL) + 
  theme(
    panel.background = element_blank(),  # Remove the panel background
    plot.background = element_blank(),   # Remove the plot background
    panel.grid = element_blank(),        # Remove the grid lines
    axis.line = element_line()           # Add axis lines back
  ) +
  geom_point(data = data_act, aes(x = age, y = shannon_shrub), color = "blue", alpha = 0.5) +
  theme(axis.title = element_text(size = 18))

#simpson
simp_gam<-gam(simpson_shrub ~ s(age), data=data_act, method="REML")
gam.check(simp_gam) #came back bad - no k adjustments could be made
k.check(simp_gam) #p < 0.05

#pielou's J
even_multi<-gam(pielou_even_shrub ~ s(age), data=data_act, method="REML")
gam.check(even_multi) #came back bad - no k adjustments could be made
k.check(even_multi) # p < 0.05

#CCF and age??
simp_age_ccf<-gam(simpson_ground ~ s(mean_closure) + s(age), data=data_act, method="REML")
gam.check(simp_age_ccf)
summary(simp_age_ccf)
concurvity(simp_age_ccf)

par(mfrow = c(1,2))
plot(simp_age_ccf, all.terms = TRUE)

rich_age_ccf<-gam(cant_richness ~ s(mean_closure) + s(age), data=data_act, method="REML")
gam.check(rich_age_ccf)
summary(rich_age_ccf)
concurvity(rich_age_ccf)
plot(rich_age_ccf)

par(mfrow = c(1,2))

plot(rich_age_ccf, all.terms=TRUE)
