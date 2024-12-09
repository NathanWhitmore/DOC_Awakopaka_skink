library(unmarked)
library(MASS)
library(tidyverse)


set.seed(487)

# parameters
n_sites = 36
psi = 0.21
p = 0.09 #0.41 


store <- NULL
warehouse <- NULL


for (q in 1:9) {
  
  n_surveys = q +1
  
  
  for (r in 1:1000) {
    print(paste(r, q))
    
    # are the sites initially occupied ?
    Z <- rbinom(n_sites, 1, psi)
    
    # make empty matrix for encounters
    Y <- matrix(NA, nrow = n_sites, ncol = n_surveys)
    
    # populate Y with binomial
    for (i in 1:n_sites) {
      for (j in 1:n_surveys) {
        Y[i, j] <- rbinom(1, 1, Z[i] * p)  # Conditional on occupancy
      }
    }
    
    # check raw data
    Y
    
    # Create an unmarked frame with the simulated data
    umf <- unmarkedFrameOccu(y = Y)
    
    # model
    model <- occu( ~ 1 ~ 1, data = umf)
    
    # get coefficients
    mod.psi <- coef(model)[1] %>% plogis()
    mod.p <- coef(model)[2] %>% plogis()
    
    store[[r]] <- data.frame (mod.p, mod.psi, n_surveys )
    
    
  }
  
  warehouse[[q]] <- bind_rows(store)
}


outcome <- bind_rows(warehouse)



ggplot()+
  theme_bw()+
  geom_violin(data = outcome, aes(x= as.factor(n_surveys), y= mod.p), 
              fill = "#ed2166", colour = "#ed2166")+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size =16))+
  theme(axis.text = element_text(size =12))+
  xlab("\nNumber of repeat surveys\n")+
  ylab("Detection probability (p)\n")+
  ylim(0,1)+
  geom_hline(yintercept = 0.09, linetype = "dotted")


ggsave("Occupancy detection.png", height = 6, width = 6, scale = 1.2)


ggplot()+
  theme_bw()+
  geom_violin(data = outcome, aes(x= as.factor(n_surveys), y= mod.psi), 
              fill = "#e45626", colour = "#e45626")+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size =16))+
  theme(axis.text = element_text(size =12))+
  xlab("\nNumber of repeat surveys\n")+
  ylab("Occupancy probability (psi)\n")+
  ylim(0,1)+
  geom_hline(yintercept = 0.21, linetype = "dotted")


ggsave("Occupancy psi.png", height = 6, width = 6, scale = 1.2)


head(outcome)

zero <- outcome  %>% filter(n_surveys == "10") %>% arrange(desc(mod.psi))

low <- outcome  %>% filter(mod.p >= 0.03) 


ggplot()+
  theme_bw()+
  geom_violin(data = low, aes(x= as.factor(n_surveys), y= mod.psi), 
              fill = "#e45626", colour = "#e45626")+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(size =16))+
  theme(axis.text = element_text(size =12))+
  xlab("\nNumber of repeat surveys\n")+
  ylab("Occupancy probability (psi)\n")+
  ylim(0,1)