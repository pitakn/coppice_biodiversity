library(gridExtra)

# boxplots for biodiversity indices in abandoned cants

indices<-read.csv("indices_final.csv")
View(indices)

indices$age<-as.numeric(indices$age)

active<-indices %>%
  filter(abandoned != "y")
View(active)

active_g<-active %>%
  filter(layer !="shrub")
str(active_g)

shan_g<-ggplot(data = active_g, aes(x=age, y=shannon)) +
  geom_point(data = active_g, aes(x=age, y=shannon), color="cyan", size = 3) +
  labs(x = "Cant age", y = "Shannon diversity (H')") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

simp_g<-ggplot(data = active_g, aes(x=age, y=simpson)) +
  geom_point(data = active_g, aes(x=age, y=simpson), color="salmon", size = 3) +
  labs(x = "Cant age", y = "Simpson diversity (1-D)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

even_g<-ggplot(data = active_g, aes(x=age, y=evenness)) +
  geom_point(data = active_g, aes(x=age, y=evenness), color="gold", size = 3) +
  labs(x = "Cant age", y = "Pielou's index of evenness (J')") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

rich_g<-ggplot(data = active_g, aes(x=age, y=richness)) +
  geom_point(data = active_g, aes(x=age, y=richness), color="#a18ff2", size = 3) +
  labs(x = "Cant age", y = "Species richness (n)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

grid.arrange(shan_g, simp_g, even_g, rich_g, nrow=2)

active_s<-active %>%
  filter(layer !="ground")
View(active_s)

shan_s<-ggplot(data = active_s, aes(x=age, y=shannon)) +
  geom_point(data = active_s, aes(x=age, y=shannon), color="cyan", size = 3) +
  labs(x = "Cant age", y = "Shannon diversity (H')") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

simp_s<-ggplot(data = active_s, aes(x=age, y=simpson)) +
  geom_point(data = active_s, aes(x=age, y=simpson), color="salmon", size = 3) +
  labs(x = "Cant age", y = "Simpson diversity (1-D)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

even_s<-ggplot(data = active_s, aes(x=age, y=evenness)) +
  geom_point(data = active_s, aes(x=age, y=evenness), color="gold", size = 3) +
  labs(x = "Cant age", y = "Pielou's index of evenness (J')") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

rich_s<-ggplot(data = active_s, aes(x=age, y=richness)) +
  geom_point(data = active_s, aes(x=age, y=richness), color="#a18ff2", size = 3) +
  labs(x = "Cant age", y = "Species richness (n)") +
  theme_minimal() +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # remove major gridlines
    panel.grid.minor = element_blank()   # remove minor gridlines
  ) +
  geom_hline(yintercept = 0, color = "grey50") +  # horizontal center line
  geom_vline(xintercept = 0, color = "grey50") + # vertical center line
  theme(legend.position = "right")

grid.arrange(shan_s, simp_s, even_s, rich_s, nrow=2)


#ABANDONED CANTS
aban<-indices %>%
  filter(abandoned != "n")

aban_gr<-aban %>%
  filter(layer != "shrub")
View(aban_gr)
aban_gr_23<-aban_gr %>%
  filter(year != "2024")
aban_gr_24<-aban_gr %>%
  filter(year != "2023")
par(mfrow=c(1,8))
boxplot(aban_gr_23$shannon, ylim=range(c(0.1, 1.6)), col=c("#40d1d6"),
        ylab="Shannon diversity (H')", xlab = "2023", las = 1)
boxplot(aban_gr_24$shannon, ylim=range(c(0.1, 1.6)), col=c("#40d1d6"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_gr_23$simpson, ylim=range(c(0, 1)), col=c("#d48194"),
        ylab="Simpson diversity (1-D)", xlab = "2023", las = 1)
boxplot(aban_gr_24$simpson, ylim=range(c(0, 1)), col=c("#d48194"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_gr_23$evenness, ylim=range(c(0.1, 0.8)), col=c("gold"),
        ylab="Pielou's index of evenness (J')", xlab = "2023", las = 1)
boxplot(aban_gr_24$evenness, ylim=range(c(0.1, 0.8)), col=c("gold"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_gr_23$richness, ylim=range(c(4,18)), col=c("#af8be8"),
        ylab="Species richness", xlab = "2023", las = 1)
boxplot(aban_gr_24$richness, ylim=range(c(4,18)), col=c("#af8be8"),
        ylab="", xlab = "2024", las = 1)

#shrub
View(aban_sh)
aban_sh<-aban %>%
  filter(layer != "ground")
aban_sh_23<-aban_sh %>%
  filter(year != "2024")
aban_sh_24<-aban_sh %>%
  filter(year != "2023")
par(mfrow=c(1,8))
boxplot(aban_sh_23$shannon, ylim=range(c(0, 2)), col=c("#40d1d6"),
        ylab="Shannon diversity (H')", xlab = "2023", las = 1)
boxplot(aban_sh_24$shannon, ylim=range(c(0, 2)), col=c("#40d1d6"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_sh_23$simpson, ylim=range(c(0, 1)), col=c("#d48194"),
        ylab="Inverse Simpson's diversity (1/D)", xlab = "2023", las = 1)
boxplot(aban_sh_24$simpson, ylim=range(c(0, 1)), col=c("#d48194"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_sh_23$evenness, ylim=range(c(0, 0.8)), col=c("#e8a554"),
        ylab="Pielou's index of evenness (J')", xlab = "2023", las = 1)
boxplot(aban_sh_24$evenness, ylim=range(c(0, 0.8)), col=c("#e8a554"),
        ylab="", xlab = "2024", las = 1)
boxplot(aban_sh_23$richness, ylim=range(c(1, 11)), col=c("#af8be8"),
        ylab="Species richness", xlab = "2023", las = 1)
boxplot(aban_sh_24$richness, ylim=range(c(1,11)), col=c("#af8be8"),
        ylab="", xlab = "2024", las = 1)
