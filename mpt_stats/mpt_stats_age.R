library(lme4)
library(lmerTest)
library(tidyverse)
library(corrplot)
install.packages("emmeans")
library(emmeans)

setwd("~/Documents/nancelab/Data/feature_data_age")

##### Read in all the data and subset down to D_fit and D_eff1, then add variables for Age, Slice, and Video

##### P14


# For slice 1
features_P14_slice_1_vid_1 <- read.csv("features_P14_40nm_s1_v1.csv")
features_P14_slice_1_vid_2 <- read.csv("features_P14_40nm_s1_v2.csv")
features_P14_slice_1_vid_3 <- read.csv("features_P14_40nm_s1_v3.csv")
features_P14_slice_1_vid_4 <- read.csv("features_P14_40nm_s1_v4.csv")
features_P14_slice_1_vid_5 <- read.csv("features_P14_40nm_s1_v5.csv")

features_P14_slice_1_vid_1$Age <- "P14"
features_P14_slice_1_vid_1$Slice <- "P14 S1"
features_P14_slice_1_vid_1$Video <- "P14 V1"

features_P14_slice_1_vid_2$Age <- "P14"
features_P14_slice_1_vid_2$Slice <- "P14 S1"
features_P14_slice_1_vid_2$Video <- "P14 V2"

features_P14_slice_1_vid_3$Age <- "P14"
features_P14_slice_1_vid_3$Slice <- "P14 S1"
features_P14_slice_1_vid_3$Video <- "P14 V3"

features_P14_slice_1_vid_4$Age <- "P14"
features_P14_slice_1_vid_4$Slice <- "P14 S1"
features_P14_slice_1_vid_4$Video <- "P14 V4"

features_P14_slice_1_vid_5$Age <- "P14"
features_P14_slice_1_vid_5$Slice <- "P14 S1"
features_P14_slice_1_vid_5$Video <- "P14 V5"

# For slice 2
features_P14_slice_2_vid_1 <- read.csv("features_P14_40nm_s2_v1.csv")
features_P14_slice_2_vid_2 <- read.csv("features_P14_40nm_s2_v2.csv")
features_P14_slice_2_vid_3 <- read.csv("features_P14_40nm_s2_v3.csv")
features_P14_slice_2_vid_4 <- read.csv("features_P14_40nm_s2_v4.csv")
features_P14_slice_2_vid_5 <- read.csv("features_P14_40nm_s2_v5.csv")

features_P14_slice_2_vid_1$Age <- "P14"
features_P14_slice_2_vid_1$Slice <- "P14 S2"
features_P14_slice_2_vid_1$Video <- "P14 V1"

features_P14_slice_2_vid_2$Age <- "P14"
features_P14_slice_2_vid_2$Slice <- "P14 S2"
features_P14_slice_2_vid_2$Video <- "P14 V2"

features_P14_slice_2_vid_3$Age <- "P14"
features_P14_slice_2_vid_3$Slice <- "P14 S2"
features_P14_slice_2_vid_3$Video <- "P14 V3"

features_P14_slice_2_vid_4$Age <- "P14"
features_P14_slice_2_vid_4$Slice <- "P14 S2"
features_P14_slice_2_vid_4$Video <- "P14 V4"

features_P14_slice_2_vid_5$Age <- "P14"
features_P14_slice_2_vid_5$Slice <- "P14 S2"
features_P14_slice_2_vid_5$Video <- "P14 V5"

# For slice 3
features_P14_slice_3_vid_1 <- read.csv("features_P14_40nm_s3_v1.csv")
features_P14_slice_3_vid_2 <- read.csv("features_P14_40nm_s3_v2.csv")
features_P14_slice_3_vid_3 <- read.csv("features_P14_40nm_s3_v3.csv")
features_P14_slice_3_vid_4 <- read.csv("features_P14_40nm_s3_v4.csv")
features_P14_slice_3_vid_5 <- read.csv("features_P14_40nm_s3_v5.csv")

features_P14_slice_3_vid_1$Age <- "P14"
features_P14_slice_3_vid_1$Slice <- "P14 S3"
features_P14_slice_3_vid_1$Video <- "P14 V1"

features_P14_slice_3_vid_2$Age <- "P14"
features_P14_slice_3_vid_2$Slice <- "P14 S3"
features_P14_slice_3_vid_2$Video <- "P14 V2"

features_P14_slice_3_vid_3$Age <- "P14"
features_P14_slice_3_vid_3$Slice <- "P14 S3"
features_P14_slice_3_vid_3$Video <- "P14 V3"

features_P14_slice_3_vid_4$Age <- "P14"
features_P14_slice_3_vid_4$Slice <- "P14 S3"
features_P14_slice_3_vid_4$Video <- "P14 V4"

features_P14_slice_3_vid_5$Age <- "P14"
features_P14_slice_3_vid_5$Slice <- "P14 S3"
features_P14_slice_3_vid_5$Video <- "P14 V5"

##### P35


# For slice 1
features_P35_slice_1_vid_1 <- read.csv("features_P35_brain_2_slice_1_vid_1.csv")
features_P35_slice_1_vid_2 <- read.csv("features_P35_brain_2_slice_1_vid_2.csv")
features_P35_slice_1_vid_3 <- read.csv("features_P35_brain_2_slice_1_vid_3.csv")
features_P35_slice_1_vid_4 <- read.csv("features_P35_brain_2_slice_1_vid_4.csv")
features_P35_slice_1_vid_5 <- read.csv("features_P35_brain_2_slice_1_vid_5.csv")

features_P35_slice_1_vid_1$Age <- "P35"
features_P35_slice_1_vid_1$Slice <- "P35 S1"
features_P35_slice_1_vid_1$Video <- "P35 V1"

features_P35_slice_1_vid_2$Age <- "P35"
features_P35_slice_1_vid_2$Slice <- "P35 S1"
features_P35_slice_1_vid_2$Video <- "P35 V2"

features_P35_slice_1_vid_3$Age <- "P35"
features_P35_slice_1_vid_3$Slice <- "P35 S1"
features_P35_slice_1_vid_3$Video <- "P35 V3"

features_P35_slice_1_vid_4$Age <- "P35"
features_P35_slice_1_vid_4$Slice <- "P35 S1"
features_P35_slice_1_vid_4$Video <- "P35 V4"

features_P35_slice_1_vid_5$Age <- "P35"
features_P35_slice_1_vid_5$Slice <- "P35 S1"
features_P35_slice_1_vid_5$Video <- "P35 V5"

# For slice 2
features_P35_slice_2_vid_1 <- read.csv("features_P35_brain_2_slice_2_vid_1.csv")
features_P35_slice_2_vid_2 <- read.csv("features_P35_brain_2_slice_2_vid_2.csv")
features_P35_slice_2_vid_3 <- read.csv("features_P35_brain_2_slice_2_vid_3.csv")
features_P35_slice_2_vid_4 <- read.csv("features_P35_brain_2_slice_2_vid_4.csv")
features_P35_slice_2_vid_5 <- read.csv("features_P35_brain_2_slice_2_vid_5.csv")

features_P35_slice_2_vid_1$Age <- "P35"
features_P35_slice_2_vid_1$Slice <- "P35 S2"
features_P35_slice_2_vid_1$Video <- "P35 V1"

features_P35_slice_2_vid_2$Age <- "P35"
features_P35_slice_2_vid_2$Slice <- "P35 S2"
features_P35_slice_2_vid_2$Video <- "P35 V2"

features_P35_slice_2_vid_3$Age <- "P35"
features_P35_slice_2_vid_3$Slice <- "P35 S2"
features_P35_slice_2_vid_3$Video <- "P35 V3"

features_P35_slice_2_vid_4$Age <- "P35"
features_P35_slice_2_vid_4$Slice <- "P35 S2"
features_P35_slice_2_vid_4$Video <- "P35 V4"

features_P35_slice_2_vid_5$Age <- "P35"
features_P35_slice_2_vid_5$Slice <- "P35 S2"
features_P35_slice_2_vid_5$Video <- "P35 V5"

# For slice 3
features_P35_slice_3_vid_1 <- read.csv("features_P35_brain_2_slice_3_vid_1.csv")
features_P35_slice_3_vid_2 <- read.csv("features_P35_brain_2_slice_3_vid_2.csv")
features_P35_slice_3_vid_3 <- read.csv("features_P35_brain_2_slice_3_vid_3.csv")
features_P35_slice_3_vid_4 <- read.csv("features_P35_brain_2_slice_3_vid_4.csv")
features_P35_slice_3_vid_5 <- read.csv("features_P35_brain_2_slice_3_vid_5.csv")

features_P35_slice_3_vid_1$Age <- "P35"
features_P35_slice_3_vid_1$Slice <- "P35 S3"
features_P35_slice_3_vid_1$Video <- "P35 V1"

features_P35_slice_3_vid_2$Age <- "P35"
features_P35_slice_3_vid_2$Slice <- "P35 S3"
features_P35_slice_3_vid_2$Video <- "P35 V2"

features_P35_slice_3_vid_3$Age <- "P35"
features_P35_slice_3_vid_3$Slice <- "P35 S3"
features_P35_slice_3_vid_3$Video <- "P35 V3"

features_P35_slice_3_vid_4$Age <- "P35"
features_P35_slice_3_vid_4$Slice <- "P35 S3"
features_P35_slice_3_vid_4$Video <- "P35 V4"

features_P35_slice_3_vid_5$Age <- "P35"
features_P35_slice_3_vid_5$Slice <- "P35 S3"
features_P35_slice_3_vid_5$Video <- "P35 V5"

##### P70


# For slice 1
features_P70_slice_1_vid_1 <- read.csv("features_P70_40nm_s1_v1.csv")
features_P70_slice_1_vid_2 <- read.csv("features_P70_40nm_s1_v2.csv")
features_P70_slice_1_vid_3 <- read.csv("features_P70_40nm_s1_v3.csv")
features_P70_slice_1_vid_4 <- read.csv("features_P70_40nm_s1_v4.csv")
features_P70_slice_1_vid_5 <- read.csv("features_P70_40nm_s1_v5.csv")

features_P70_slice_1_vid_1$Age <- "P70"
features_P70_slice_1_vid_1$Slice <- "P70 S1"
features_P70_slice_1_vid_1$Video <- "P70 V1"

features_P70_slice_1_vid_2$Age <- "P70"
features_P70_slice_1_vid_2$Slice <- "P70 S1"
features_P70_slice_1_vid_2$Video <- "P70 V2"

features_P70_slice_1_vid_3$Age <- "P70"
features_P70_slice_1_vid_3$Slice <- "P70 S1"
features_P70_slice_1_vid_3$Video <- "P70 V3"

features_P70_slice_1_vid_4$Age <- "P70"
features_P70_slice_1_vid_4$Slice <- "P70 S1"
features_P70_slice_1_vid_4$Video <- "P70 V4"

features_P70_slice_1_vid_5$Age <- "P70"
features_P70_slice_1_vid_5$Slice <- "P70 S1"
features_P70_slice_1_vid_5$Video <- "P70 V5"

# For slice 2
features_P70_slice_2_vid_1 <- read.csv("features_P70_40nm_s2_v1.csv")
features_P70_slice_2_vid_2 <- read.csv("features_P70_40nm_s2_v2.csv")
features_P70_slice_2_vid_3 <- read.csv("features_P70_40nm_s2_v3.csv")
features_P70_slice_2_vid_4 <- read.csv("features_P70_40nm_s2_v4.csv")
features_P70_slice_2_vid_5 <- read.csv("features_P70_40nm_s2_v5.csv")

features_P70_slice_2_vid_1$Age <- "P70"
features_P70_slice_2_vid_1$Slice <- "P70 S2"
features_P70_slice_2_vid_1$Video <- "P70 V1"

features_P70_slice_2_vid_2$Age <- "P70"
features_P70_slice_2_vid_2$Slice <- "P70 S2"
features_P70_slice_2_vid_2$Video <- "P70 V2"

features_P70_slice_2_vid_3$Age <- "P70"
features_P70_slice_2_vid_3$Slice <- "P70 S2"
features_P70_slice_2_vid_3$Video <- "P70 V3"

features_P70_slice_2_vid_4$Age <- "P70"
features_P70_slice_2_vid_4$Slice <- "P70 S2"
features_P70_slice_2_vid_4$Video <- "P70 V4"

features_P70_slice_2_vid_5$Age <- "P70"
features_P70_slice_2_vid_5$Slice <- "P70 S2"
features_P70_slice_2_vid_5$Video <- "P70 V5"

# For slice 3
features_P70_slice_3_vid_1 <- read.csv("features_P70_40nm_s3_v1.csv")
features_P70_slice_3_vid_2 <- read.csv("features_P70_40nm_s3_v2.csv")
features_P70_slice_3_vid_3 <- read.csv("features_P70_40nm_s3_v3.csv")
features_P70_slice_3_vid_4 <- read.csv("features_P70_40nm_s3_v4.csv")
features_P70_slice_3_vid_5 <- read.csv("features_P70_40nm_s3_v5.csv")

features_P70_slice_3_vid_1$Age <- "P70"
features_P70_slice_3_vid_1$Slice <- "P70 S3"
features_P70_slice_3_vid_1$Video <- "P70 V1"

features_P70_slice_3_vid_2$Age <- "P70"
features_P70_slice_3_vid_2$Slice <- "P70 S3"
features_P70_slice_3_vid_2$Video <- "P70 V2"

features_P70_slice_3_vid_3$Age <- "P70"
features_P70_slice_3_vid_3$Slice <- "P70 S3"
features_P70_slice_3_vid_3$Video <- "P70 V3"

features_P70_slice_3_vid_4$Age <- "P70"
features_P70_slice_3_vid_4$Slice <- "P70 S3"
features_P70_slice_3_vid_4$Video <- "P70 V4"

features_P70_slice_3_vid_5$Age <- "P70"
features_P70_slice_3_vid_5$Slice <- "P70 S3"
features_P70_slice_3_vid_5$Video <- "P70 V5"


mpt <- bind_rows(
  features_P14_slice_1_vid_1,
  features_P14_slice_1_vid_2,
  features_P14_slice_1_vid_3,
  features_P14_slice_1_vid_4,
  features_P14_slice_1_vid_5,
  features_P14_slice_2_vid_1,
  features_P14_slice_2_vid_2,
  features_P14_slice_2_vid_3,
  features_P14_slice_2_vid_4,
  features_P14_slice_2_vid_5,
  features_P14_slice_3_vid_1,
  features_P14_slice_3_vid_2,
  features_P14_slice_3_vid_3,
  features_P14_slice_3_vid_4,
  features_P14_slice_3_vid_5,
  features_P35_slice_1_vid_1,
  features_P35_slice_1_vid_2,
  features_P35_slice_1_vid_3,
  features_P35_slice_1_vid_4,
  features_P35_slice_1_vid_5,
  features_P35_slice_2_vid_1,
  features_P35_slice_2_vid_2,
  features_P35_slice_2_vid_3,
  features_P35_slice_2_vid_4,
  features_P35_slice_2_vid_5,
  features_P35_slice_3_vid_1,
  features_P35_slice_3_vid_2,
  features_P35_slice_3_vid_3,
  features_P35_slice_3_vid_4,
  features_P35_slice_3_vid_5,
  features_P70_slice_1_vid_1,
  features_P70_slice_1_vid_2,
  features_P70_slice_1_vid_3,
  features_P70_slice_1_vid_4,
  features_P70_slice_1_vid_5,
  features_P70_slice_2_vid_1,
  features_P70_slice_2_vid_2,
  features_P70_slice_2_vid_3,
  features_P70_slice_2_vid_4,
  features_P70_slice_2_vid_5,
  features_P70_slice_3_vid_1,
  features_P70_slice_3_vid_2,
  features_P70_slice_3_vid_3,
  features_P70_slice_3_vid_4,
  features_P70_slice_3_vid_5,
)

mpt$Age <- as.factor(mpt$Age)

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


plot_data = mpt[c('Mean.fractal_', 'Slice', 'Age', 'frames')]
plot_data <- na.omit(plot_data)
plot_data$Mean.kurtosis <- plot_data$Mean.MSD_ratio+1
ggplot(plot_data, aes(x=(Mean.boundedness)))+
  geom_histogram()

# m1 = lmer((Mean.straightness) ~ relevel(Age, ref="P14") + frames + (1|Slice), data=plot_data)
# m0 = lmer((Mean.straightness) ~ frames + (1|Slice) + (1|frames), data=plot_data)
# 
# anova(m0,m1)
# summary(m1)
# confint(m1)

m1 = lmer((Mean.boundedness) ~ Age + (1|Slice), data=mpt) #Assumes Age is a 3-level factor with levels P14, P35, and P70
summary(m1)
emm <- emmeans(m1, ~ Age)
age_contrast <- contrast(emm, method = list("str_vs_hippo" = c(0, 1, -1))) #Sets the values of the levels to 0,  1, and -1 therfore giving you the difference between level 2 (P35) and level 3 (P70).
age_contrast
confint(age_contrast)
emm

p <- ggplot(mpt, aes(x = Age, y = Mean.boundedness, fill = Age)) +
  geom_violin(alpha = 1, trim=TRUE, colour= NA, show.legend = FALSE) +
  
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               colour = "black")+
  coord_cartesian(ylim = c(0, 0.5))+
  scale_y_continuous(breaks = seq(0, 0.5, by=.1))+
  #theme_minimal()+
  labs(y="Mean boundedness")+
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

base_value <- 0.4
modifier <-0.025

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
  annotate("text", x = 1.5, y = base_value+1*modifier, label = "ns", size = 6) +  # Add significance stars between 1 and 2
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
