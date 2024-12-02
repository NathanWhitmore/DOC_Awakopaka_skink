library(tidyverse)

# naive detection)
naive  <- 4 / 208

# real as estimate
p <- seq(from = 0, to = 1, by = 0.01)

# estimate p
est <- data.frame(p, naive)
est$psi <- est$naive/ est$p
est

# which estimate approximates p = 0.09
prior <- est[which(est$p == 0.09), ]  

# plot
ggplot()+
  theme_bw()+
  geom_line(data = est, aes(x= p, y= psi), lwd = 2, colour = "firebrick")+
  geom_hline(data = prior, aes(yintercept= p) , colour = "red")+
  geom_vline(data = prior, aes(xintercept= psi) , colour = "red")+
  ylim(0, 1)+
  ylab("Detection (p)\n")+
  xlab("\nOccupancy (psi)")+
  theme(axis.text = element_text(size = 12))+
  theme(axis.title = element_text(size = 16))+
  ggtitle("Likely vicinity of detection probability")+
  labs(subtitle = "p = 0.09 and psi = 0.21 results in 0.019 (4/208)")

# Assumption:
# Awakopaka skinks must be characterised by lower occupancy and low detection
ggsave("Detection probaility.png", scale = 1.2)




