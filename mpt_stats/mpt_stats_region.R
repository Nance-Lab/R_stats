library(lme4)
library(lmerTest)
library(tidyverse)
library(corrplot)
library(emmeans)
#install.packages("ggsignif")
library(ggsignif)
setwd("~/Documents/nancelab/Data/region_feature_folder")

# CORTEX

# For slice 1
features_cortex_slice_1_vid_1 <- read.csv("feat_NT_slice_1_cortex_vid_6.csv")
features_cortex_slice_1_vid_2 <- read.csv("feat_NT_slice_1_cortex_vid_7.csv")
features_cortex_slice_1_vid_3 <- read.csv("feat_NT_slice_1_cortex_vid_8.csv")
features_cortex_slice_1_vid_4 <- read.csv("feat_NT_slice_1_cortex_vid_9.csv")
features_cortex_slice_1_vid_5 <- read.csv("feat_NT_slice_1_cortex_vid_10.csv")

features_cortex_slice_1_vid_1$Region <- "cortex"
features_cortex_slice_1_vid_1$Slice <- "S1"
features_cortex_slice_1_vid_1$Video <- "cortex V1"

features_cortex_slice_1_vid_2$Region <- "cortex"
features_cortex_slice_1_vid_2$Slice <- "S1"
features_cortex_slice_1_vid_2$Video <- "cortex V2"

features_cortex_slice_1_vid_3$Region <- "cortex"
features_cortex_slice_1_vid_3$Slice <- "S1"
features_cortex_slice_1_vid_3$Video <- "cortex V3"

features_cortex_slice_1_vid_4$Region <- "cortex"
features_cortex_slice_1_vid_4$Slice <- "S1"
features_cortex_slice_1_vid_4$Video <- "cortex V4"

features_cortex_slice_1_vid_5$Region <- "cortex"
features_cortex_slice_1_vid_5$Slice <- "S1"
features_cortex_slice_1_vid_5$Video <- "cortex V5"

# For slice 2
features_cortex_slice_2_vid_1 <- read.csv("feat_NT_slice_2_cortex_vid_1.csv")
features_cortex_slice_2_vid_2 <- read.csv("feat_NT_slice_2_cortex_vid_2.csv")
features_cortex_slice_2_vid_3 <- read.csv("feat_NT_slice_2_cortex_vid_3.csv")
features_cortex_slice_2_vid_4 <- read.csv("feat_NT_slice_2_cortex_vid_4.csv")
features_cortex_slice_2_vid_5 <- read.csv("feat_NT_slice_2_cortex_vid_5.csv")

features_cortex_slice_2_vid_1$Region <- "cortex"
features_cortex_slice_2_vid_1$Slice <- "S1"
features_cortex_slice_2_vid_1$Video <- "cortex V1"

features_cortex_slice_2_vid_2$Region <- "cortex"
features_cortex_slice_2_vid_2$Slice <- "S1"
features_cortex_slice_2_vid_2$Video <- "cortex V2"

features_cortex_slice_2_vid_3$Region <- "cortex"
features_cortex_slice_2_vid_3$Slice <- "S1"
features_cortex_slice_2_vid_3$Video <- "cortex V3"

features_cortex_slice_2_vid_4$Region <- "cortex"
features_cortex_slice_2_vid_4$Slice <- "S1"
features_cortex_slice_2_vid_4$Video <- "cortex V4"

features_cortex_slice_2_vid_5$Region <- "cortex"
features_cortex_slice_2_vid_5$Slice <- "S1"
features_cortex_slice_2_vid_5$Video <- "cortex V5"

# STRIATUM

# For slice 1
features_striatum_slice_1_vid_1 <- read.csv("feat_NT_slice_1_striatum_vid_1.csv")
features_striatum_slice_1_vid_2 <- read.csv("feat_NT_slice_1_striatum_vid_2.csv")
features_striatum_slice_1_vid_3 <- read.csv("feat_NT_slice_1_striatum_vid_3.csv")
features_striatum_slice_1_vid_4 <- read.csv("feat_NT_slice_1_striatum_vid_4.csv")
features_striatum_slice_1_vid_5 <- read.csv("feat_NT_slice_1_striatum_vid_5.csv")

features_striatum_slice_1_vid_1$Region <- "striatum"
features_striatum_slice_1_vid_1$Slice <- "S1"
features_striatum_slice_1_vid_1$Video <- "striatum V1"

features_striatum_slice_1_vid_2$Region <- "striatum"
features_striatum_slice_1_vid_2$Slice <- "S1"
features_striatum_slice_1_vid_2$Video <- "striatum V2"

features_striatum_slice_1_vid_3$Region <- "striatum"
features_striatum_slice_1_vid_3$Slice <- "S1"
features_striatum_slice_1_vid_3$Video <- "striatum V3"

features_striatum_slice_1_vid_4$Region <- "striatum"
features_striatum_slice_1_vid_4$Slice <- "S1"
features_striatum_slice_1_vid_4$Video <- "striatum V4"

features_striatum_slice_1_vid_5$Region <- "striatum"
features_striatum_slice_1_vid_5$Slice <- "S1"
features_striatum_slice_1_vid_5$Video <- "striatum V5"

# For slice 2
features_striatum_slice_2_vid_1 <- read.csv("feat_NT_slice_2_striatum_vid_1.csv")
features_striatum_slice_2_vid_2 <- read.csv("feat_NT_slice_2_striatum_vid_2.csv")
features_striatum_slice_2_vid_3 <- read.csv("feat_NT_slice_2_striatum_vid_3.csv")
features_striatum_slice_2_vid_4 <- read.csv("feat_NT_slice_2_striatum_vid_4.csv")
features_striatum_slice_2_vid_5 <- read.csv("feat_NT_slice_2_striatum_vid_5.csv")

features_striatum_slice_2_vid_1$Region <- "striatum"
features_striatum_slice_2_vid_1$Slice <- "S2"
features_striatum_slice_2_vid_1$Video <- "striatum V1"

features_striatum_slice_2_vid_2$Region <- "striatum"
features_striatum_slice_2_vid_2$Slice <- "S2"
features_striatum_slice_2_vid_2$Video <- "striatum V2"

features_striatum_slice_2_vid_3$Region <- "striatum"
features_striatum_slice_2_vid_3$Slice <- "S2"
features_striatum_slice_2_vid_3$Video <- "striatum V3"

features_striatum_slice_2_vid_4$Region <- "striatum"
features_striatum_slice_2_vid_4$Slice <- "S2"
features_striatum_slice_2_vid_4$Video <- "striatum V4"

features_striatum_slice_2_vid_5$Region <- "striatum"
features_striatum_slice_2_vid_5$Slice <- "S2"
features_striatum_slice_2_vid_5$Video <- "striatum V5"

# HIPPOCAMPUS

# For slice 1
features_hippocampus_slice_1_vid_1 <- read.csv("feat_NT_slice_1_hippocampus_vid_1.csv")
features_hippocampus_slice_1_vid_2 <- read.csv("feat_NT_slice_1_hippocampus_vid_2.csv")
features_hippocampus_slice_1_vid_3 <- read.csv("feat_NT_slice_1_hippocampus_vid_3.csv")


features_hippocampus_slice_1_vid_1$Region <- "hippocampus"
features_hippocampus_slice_1_vid_1$Slice <- "S1"
features_hippocampus_slice_1_vid_1$Video <- "hippocampus V1"

features_hippocampus_slice_1_vid_2$Region <- "hippocampus"
features_hippocampus_slice_1_vid_2$Slice <- "S1"
features_hippocampus_slice_1_vid_2$Video <- "hippocampus V2"

features_hippocampus_slice_1_vid_3$Region <- "hippocampus"
features_hippocampus_slice_1_vid_3$Slice <- "S1"
features_hippocampus_slice_1_vid_3$Video <- "hippocampus V3"



# For slice 2
features_hippocampus_slice_2_vid_1 <- read.csv("feat_NT_slice_2_hippocampus_vid_1.csv")
features_hippocampus_slice_2_vid_2 <- read.csv("feat_NT_slice_2_hippocampus_vid_2.csv")
features_hippocampus_slice_2_vid_3 <- read.csv("feat_NT_slice_2_hippocampus_vid_3.csv")

features_hippocampus_slice_2_vid_1$Region <- "hippocampus"
features_hippocampus_slice_2_vid_1$Slice <- "S2"
features_hippocampus_slice_2_vid_1$Video <- "hippocampus V1"

features_hippocampus_slice_2_vid_2$Region <- "hippocampus"
features_hippocampus_slice_2_vid_2$Slice <- "S2"
features_hippocampus_slice_2_vid_2$Video <- "hippocampus V2"

features_hippocampus_slice_2_vid_3$Region <- "hippocampus"
features_hippocampus_slice_2_vid_3$Slice <- "S2"
features_hippocampus_slice_2_vid_3$Video <- "hippocampus V3"

mpt <- bind_rows(
  features_cortex_slice_1_vid_1,
  features_cortex_slice_1_vid_2,
  features_cortex_slice_1_vid_3,
  features_cortex_slice_1_vid_4,
  features_cortex_slice_1_vid_5,
  features_cortex_slice_2_vid_1,
  features_cortex_slice_2_vid_2,
  features_cortex_slice_2_vid_3,
  features_cortex_slice_2_vid_4,
  features_cortex_slice_2_vid_5,
  features_striatum_slice_1_vid_1,
  features_striatum_slice_1_vid_2,
  features_striatum_slice_1_vid_3,
  features_striatum_slice_1_vid_4,
  features_striatum_slice_1_vid_5,
  features_striatum_slice_2_vid_1,
  features_striatum_slice_2_vid_2,
  features_striatum_slice_2_vid_3,
  features_striatum_slice_2_vid_4,
  features_striatum_slice_2_vid_5,
  
  features_hippocampus_slice_1_vid_1,
  features_hippocampus_slice_1_vid_2,
  features_hippocampus_slice_1_vid_3,
  
  features_hippocampus_slice_2_vid_1,
  features_hippocampus_slice_2_vid_2,
  features_hippocampus_slice_2_vid_3,
 
)

mpt$Region <- as.factor(mpt$Region)

mpt[sapply(mpt, is.infinite)] <- NA

features_for_corr = mpt[c('Mean.alpha',
                          'Mean.Deff1',
                          'Mean.trappedness',
                          'Mean.MSD_ratio',
                          'Mean.asymmetry1',
                          'Mean.kurtosis',
                          'Mean.fractal_dim',
                          'Mean.straightness',
                          'Mean.efficiency',
                          'Mean.D_fit',
                          'Mean.AR',
                          'Mean.boundedness',
                          'Mean.Deff2',
                          'frames'
)]

no_nas = na.omit(features_for_corr)
cor_data = cor(no_nas)

corrplot(cor_data, method="circle", diag=FALSE, order='FPC', type='lower')

plot_data = mpt[c('Mean.Deff1', 'Slice', 'Region', 'Video', 'frames')]
plot_data <- na.omit(plot_data)
#plot_data$Mean.Deff2 <- plot_data$Mean.Deff2+1

plot_data %>% filter(Slice=="cortex S1") %>% 
  
ggplot(plot_data, aes(x=log(Mean.Deff1)))+
  geom_histogram()



m1 = lmer((Mean.Deff1) ~ Region + (1|Slice), data=mpt) #Assumes Age is a 3-level factor with levels P14, P35, and P70
summary(m1)
emm <- emmeans(m1, ~ Region)
age_contrast <- contrast(emm, method = list("str_vs_hippo" = c(0, 1, -1))) #Sets the values of the levels to 0,  1, and -1 therfore giving you the difference between level 2 (P35) and level 3 (P70).
age_contrast
confint(age_contrast)
emm

clean_and_mean_by_treatment <- function(mpt, column_name) {
  mpt %>%
    group_by(Region) %>%
    filter(is.finite(.data[[column_name]])) %>%
    summarize(mean = mean(.data[[column_name]], na.rm = TRUE))
}

out = clean_and_mean_by_treatment(mpt, "Mean.boundedness")
out
confint(m1)
confint(age_contrast)

p <- ggplot(mpt, aes(x = Region, y = Mean.Deff1, fill = Region)) +
  geom_violin(alpha = 1, trim=TRUE, colour= NA, show.legend = FALSE) +
  
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               colour = "black")+
  coord_cartesian(ylim = c(1,5))+
  scale_y_continuous(breaks = seq(1, 5, by=0.5))+
  #theme_minimal()+
  labs(y="Mean fractal_dimt")+
  theme(
    text = element_text(family="Arial"),
    axis.title.y = element_text(size=18, face="bold", family="Arial"),
    axis.title.x = element_text(size=18, face="bold", family="Arial"),
    axis.text.x = element_text(size=18, face="bold", family="Arial"),
    #legend.text = element_text(size=18, face="bold", family="Arial"),
    axis.text.y = element_text(size=18, face="bold", family="Arial"),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    #axis.line = element_line(colour = "black")
  ) + guides()

p 

base_value <- 4.5
modifier <-0.15

 p +
   theme(legend.position = "none")+
 
#   annotate("text", x=2.0, y=5.75, label = "*", color="black", size=10, fontface="bold")+
#   annotate("text", x=3.0, y=5.75, label = "*", color="black", size=10, fontface="bold")+
 # # # Add significance bars
   geom_segment(aes(x = 1, xend = 1.95, y = base_value, yend = base_value), size = 0.5) +  # Bar between Region 1 and 2
   #geom_segment(aes(x = 1, xend = 1, y = 28, yend = 26), size = 0.5) +  # Vertical tick for Region 1
   #geom_segment(aes(x = 2, xend = 2, y = 28, yend = 26), size = 0.5) +  # Vertical tick for Region 2
 # #   
 # #   # Add annotation for the significance level
   annotate("text", x = 1.5, y = base_value+1*modifier, label = "***", size = 6) +  # Add significance stars between 1 and 2
 # #   
 # #   # You can repeat this process for other comparisons
    geom_segment(aes(x = 2.05, xend = 3, y = base_value, yend = base_value), size = 0.5) +  # Bar between Region 2 and 3
 #   geom_segment(aes(x = 2, xend = 2, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 2
 #   geom_segment(aes(x = 3, xend = 3, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 3
 # #   
     annotate("text", x = 2.5, y = base_value+1*modifier, label = "***", size = 6) + # Add significance stars between 2 and 3
 # # 
 # # # You can repeat this process for other comparisons
    geom_segment(aes(x = 1, xend = 3, y = base_value+2*modifier, yend = base_value+2*modifier), size = 0.5) +  # Bar between Region 2 and 3
 #   geom_segment(aes(x = 1, xend = 1, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 2
 #   geom_segment(aes(x = 3, xend = 3, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 3
 # #   
     annotate("text", x = 2, y = base_value+3*modifier, label = "***", size = 6)  # Add significance stars between 2 and 3
 
