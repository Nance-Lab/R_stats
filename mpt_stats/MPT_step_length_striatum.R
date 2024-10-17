library(lme4)
library(lmerTest)
library(tidyverse)
library(corrplot)

#setwd("Users/nelsschimek/Documents/nancelab/Data/OGD_severity/") ### Change this to whatever folder you save things to

##### Read in all the data and subset down to D_fit and D_eff1, then add variables for Age, Slice, and Video
##### NT

setwd("~/Documents/nancelab/Data/OGD_severity")

# For slice 1
feat_NT_slice_1_striatum_vid_1 <- read.csv("feat_NT_slice_1_striatum_vid_1.csv")
feat_NT_slice_1_striatum_vid_2 <- read.csv("feat_NT_slice_1_striatum_vid_2.csv")
feat_NT_slice_1_striatum_vid_3 <- read.csv("feat_NT_slice_1_striatum_vid_3.csv")
feat_NT_slice_1_striatum_vid_4 <- read.csv("feat_NT_slice_1_striatum_vid_4.csv")
feat_NT_slice_1_striatum_vid_5 <- read.csv("feat_NT_slice_1_striatum_vid_4.csv")

feat_NT_slice_1_striatum_vid_1$Treatment <- "HC"
feat_NT_slice_1_striatum_vid_1$Slice <- "HC S1"
feat_NT_slice_1_striatum_vid_1$Video <- "HC V1"

feat_NT_slice_1_striatum_vid_2$Treatment <- "HC"
feat_NT_slice_1_striatum_vid_2$Slice <- "HC S1"
feat_NT_slice_1_striatum_vid_2$Video <- "HC V2"

feat_NT_slice_1_striatum_vid_3$Treatment <- "HC"
feat_NT_slice_1_striatum_vid_3$Slice <- "HC S1"
feat_NT_slice_1_striatum_vid_3$Video <- "HC V3"

feat_NT_slice_1_striatum_vid_4$Treatment <- "HC"
feat_NT_slice_1_striatum_vid_4$Slice <- "HC S1"
feat_NT_slice_1_striatum_vid_4$Video <- "HC V4"

feat_NT_slice_1_striatum_vid_5$Treatment <- "HC"
feat_NT_slice_1_striatum_vid_5$Slice <- "HC S1"
feat_NT_slice_1_striatum_vid_5$Video <- "HC V5"

# For slice 2
feat_NT_slice_2_striatum_vid_1 <- read.csv("feat_NT_slice_2_striatum_vid_1.csv")
feat_NT_slice_2_striatum_vid_2 <- read.csv("feat_NT_slice_2_striatum_vid_2.csv")
feat_NT_slice_2_striatum_vid_3 <- read.csv("feat_NT_slice_2_striatum_vid_3.csv")
feat_NT_slice_2_striatum_vid_4 <- read.csv("feat_NT_slice_2_striatum_vid_4.csv")
feat_NT_slice_2_striatum_vid_5 <- read.csv("feat_NT_slice_2_striatum_vid_5.csv")

feat_NT_slice_2_striatum_vid_1$Treatment <- "HC"
feat_NT_slice_2_striatum_vid_1$Slice <- "HC S2"
feat_NT_slice_2_striatum_vid_1$Video <- "HC V1"

feat_NT_slice_2_striatum_vid_2$Treatment <- "HC"
feat_NT_slice_2_striatum_vid_2$Slice <- "HC S2"
feat_NT_slice_2_striatum_vid_2$Video <- "HC V2"

feat_NT_slice_2_striatum_vid_3$Treatment <- "HC"
feat_NT_slice_2_striatum_vid_3$Slice <- "HC S2"
feat_NT_slice_2_striatum_vid_3$Video <- "HC V3"

feat_NT_slice_2_striatum_vid_4$Treatment <- "HC"
feat_NT_slice_2_striatum_vid_4$Slice <- "HC S2"
feat_NT_slice_2_striatum_vid_4$Video <- "HC V4"

feat_NT_slice_2_striatum_vid_5$Treatment <- "HC"
feat_NT_slice_2_striatum_vid_5$Slice <- "HC S2"
feat_NT_slice_2_striatum_vid_5$Video <- "HC V5"



#O.5h

# For slice 1
feat_OGD_0_5h_slice_1_striatum_vid_1 <- read.csv("features_OGD_0_5h_slice_1_striatum_vid_1.csv")
feat_OGD_0_5h_slice_1_striatum_vid_2 <- read.csv("features_OGD_0_5h_slice_1_striatum_vid_2.csv")
feat_OGD_0_5h_slice_1_striatum_vid_3 <- read.csv("features_OGD_0_5h_slice_1_striatum_vid_3.csv")
feat_OGD_0_5h_slice_1_striatum_vid_4 <- read.csv("features_OGD_0_5h_slice_1_striatum_vid_4.csv")
feat_OGD_0_5h_slice_1_striatum_vid_5 <- read.csv("features_OGD_0_5h_slice_1_striatum_vid_5.csv")

feat_OGD_0_5h_slice_1_striatum_vid_1$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_1_striatum_vid_1$Slice <- "OGD S1"
feat_OGD_0_5h_slice_1_striatum_vid_1$Video <- "OGD V1"

feat_OGD_0_5h_slice_1_striatum_vid_2$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_1_striatum_vid_2$Slice <- "OGD S1"
feat_OGD_0_5h_slice_1_striatum_vid_2$Video <- "OGD V2"

feat_OGD_0_5h_slice_1_striatum_vid_3$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_1_striatum_vid_3$Slice <- "OGD S1"
feat_OGD_0_5h_slice_1_striatum_vid_3$Video <- "OGD V3"

feat_OGD_0_5h_slice_1_striatum_vid_4$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_1_striatum_vid_4$Slice <- "OGD S1"
feat_OGD_0_5h_slice_1_striatum_vid_4$Video <- "OGD V4"

feat_OGD_0_5h_slice_1_striatum_vid_5$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_1_striatum_vid_5$Slice <- "OGD S1"
feat_OGD_0_5h_slice_1_striatum_vid_5$Video <- "OGD V5"

# For slice 2
feat_OGD_0_5h_slice_2_striatum_vid_1 <- read.csv("features_OGD_0_5h_slice_2_striatum_vid_1.csv")
feat_OGD_0_5h_slice_2_striatum_vid_2 <- read.csv("features_OGD_0_5h_slice_2_striatum_vid_2.csv")
feat_OGD_0_5h_slice_2_striatum_vid_3 <- read.csv("features_OGD_0_5h_slice_2_striatum_vid_3.csv")
feat_OGD_0_5h_slice_2_striatum_vid_4 <- read.csv("features_OGD_0_5h_slice_2_striatum_vid_4.csv")
feat_OGD_0_5h_slice_2_striatum_vid_5 <- read.csv("features_OGD_0_5h_slice_2_striatum_vid_5.csv")

feat_OGD_0_5h_slice_2_striatum_vid_1$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_2_striatum_vid_1$Slice <- "OGD S2"
feat_OGD_0_5h_slice_2_striatum_vid_1$Video <- "OGD V1"

feat_OGD_0_5h_slice_2_striatum_vid_2$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_2_striatum_vid_2$Slice <- "OGD S2"
feat_OGD_0_5h_slice_2_striatum_vid_2$Video <- "OGD V2"

feat_OGD_0_5h_slice_2_striatum_vid_3$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_2_striatum_vid_3$Slice <- "OGD S2"
feat_OGD_0_5h_slice_2_striatum_vid_3$Video <- "OGD V3"

feat_OGD_0_5h_slice_2_striatum_vid_4$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_2_striatum_vid_4$Slice <- "OGD S2"
feat_OGD_0_5h_slice_2_striatum_vid_4$Video <- "OGD V4"

feat_OGD_0_5h_slice_2_striatum_vid_5$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_2_striatum_vid_5$Slice <- "OGD S2"
feat_OGD_0_5h_slice_2_striatum_vid_5$Video <- "OGD V5"

# For slice 3
feat_OGD_0_5h_slice_3_striatum_vid_1 <- read.csv("features_OGD_0_5h_slice_3_striatum_vid_1.csv")
feat_OGD_0_5h_slice_3_striatum_vid_2 <- read.csv("features_OGD_0_5h_slice_3_striatum_vid_2.csv")
feat_OGD_0_5h_slice_3_striatum_vid_3 <- read.csv("features_OGD_0_5h_slice_3_striatum_vid_3.csv")
feat_OGD_0_5h_slice_3_striatum_vid_4 <- read.csv("features_OGD_0_5h_slice_3_striatum_vid_4.csv")
feat_OGD_0_5h_slice_3_striatum_vid_5 <- read.csv("features_OGD_0_5h_slice_3_striatum_vid_5.csv")

feat_OGD_0_5h_slice_3_striatum_vid_1$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_3_striatum_vid_1$Slice <- "OGD S3"
feat_OGD_0_5h_slice_3_striatum_vid_1$Video <- "OGD V1"

feat_OGD_0_5h_slice_3_striatum_vid_2$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_3_striatum_vid_2$Slice <- "OGD S3"
feat_OGD_0_5h_slice_3_striatum_vid_2$Video <- "OGD V2"

feat_OGD_0_5h_slice_3_striatum_vid_3$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_3_striatum_vid_3$Slice <- "OGD S3"
feat_OGD_0_5h_slice_3_striatum_vid_3$Video <- "OGD V3"

feat_OGD_0_5h_slice_3_striatum_vid_4$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_3_striatum_vid_4$Slice <- "OGD S3"
feat_OGD_0_5h_slice_3_striatum_vid_4$Video <- "OGD V4"

feat_OGD_0_5h_slice_3_striatum_vid_5$Treatment <- "OGD 0.5h"
feat_OGD_0_5h_slice_3_striatum_vid_5$Slice <- "OGD S3"
feat_OGD_0_5h_slice_3_striatum_vid_5$Video <- "OGD V5"


#1.5h 

# For slice 1
feat_OGD_1_5h_slice_1_striatum_vid_1 <- read.csv("features_OGD_1_5h_slice_1_striatum_vid_1.csv")
feat_OGD_1_5h_slice_1_striatum_vid_2 <- read.csv("features_OGD_1_5h_slice_1_striatum_vid_2.csv")
feat_OGD_1_5h_slice_1_striatum_vid_3 <- read.csv("features_OGD_1_5h_slice_1_striatum_vid_3.csv")
feat_OGD_1_5h_slice_1_striatum_vid_4 <- read.csv("features_OGD_1_5h_slice_1_striatum_vid_4.csv")
feat_OGD_1_5h_slice_1_striatum_vid_5 <- read.csv("features_OGD_1_5h_slice_1_striatum_vid_5.csv")

feat_OGD_1_5h_slice_1_striatum_vid_1$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_1_striatum_vid_1$Slice <- "OGD S1"
feat_OGD_1_5h_slice_1_striatum_vid_1$Video <- "OGD V1"

feat_OGD_1_5h_slice_1_striatum_vid_2$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_1_striatum_vid_2$Slice <- "OGD S1"
feat_OGD_1_5h_slice_1_striatum_vid_2$Video <- "OGD V2"

feat_OGD_1_5h_slice_1_striatum_vid_3$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_1_striatum_vid_3$Slice <- "OGD S1"
feat_OGD_1_5h_slice_1_striatum_vid_3$Video <- "OGD V3"

feat_OGD_1_5h_slice_1_striatum_vid_4$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_1_striatum_vid_4$Slice <- "OGD S1"
feat_OGD_1_5h_slice_1_striatum_vid_4$Video <- "OGD V4"

feat_OGD_1_5h_slice_1_striatum_vid_5$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_1_striatum_vid_5$Slice <- "OGD S1"
feat_OGD_1_5h_slice_1_striatum_vid_5$Video <- "OGD V5"

# For slice 2
feat_OGD_1_5h_slice_2_striatum_vid_1 <- read.csv("features_OGD_1_5h_slice_2_striatum_vid_1.csv")
feat_OGD_1_5h_slice_2_striatum_vid_2 <- read.csv("features_OGD_1_5h_slice_2_striatum_vid_2.csv")
feat_OGD_1_5h_slice_2_striatum_vid_3 <- read.csv("features_OGD_1_5h_slice_2_striatum_vid_3.csv")
feat_OGD_1_5h_slice_2_striatum_vid_4 <- read.csv("features_OGD_1_5h_slice_2_striatum_vid_4.csv")
feat_OGD_1_5h_slice_2_striatum_vid_5 <- read.csv("features_OGD_1_5h_slice_2_striatum_vid_5.csv")

feat_OGD_1_5h_slice_2_striatum_vid_1$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_2_striatum_vid_1$Slice <- "OGD S2"
feat_OGD_1_5h_slice_2_striatum_vid_1$Video <- "OGD V1"

feat_OGD_1_5h_slice_2_striatum_vid_2$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_2_striatum_vid_2$Slice <- "OGD S2"
feat_OGD_1_5h_slice_2_striatum_vid_2$Video <- "OGD V2"

feat_OGD_1_5h_slice_2_striatum_vid_3$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_2_striatum_vid_3$Slice <- "OGD S2"
feat_OGD_1_5h_slice_2_striatum_vid_3$Video <- "OGD V3"

feat_OGD_1_5h_slice_2_striatum_vid_4$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_2_striatum_vid_4$Slice <- "OGD S2"
feat_OGD_1_5h_slice_2_striatum_vid_4$Video <- "OGD V4"

feat_OGD_1_5h_slice_2_striatum_vid_5$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_2_striatum_vid_5$Slice <- "OGD S2"
feat_OGD_1_5h_slice_2_striatum_vid_5$Video <- "OGD V5"

# For slice 3
feat_OGD_1_5h_slice_3_striatum_vid_1 <- read.csv("features_OGD_1_5h_slice_3_striatum_vid_1.csv")
feat_OGD_1_5h_slice_3_striatum_vid_2 <- read.csv("features_OGD_1_5h_slice_3_striatum_vid_2.csv")
feat_OGD_1_5h_slice_3_striatum_vid_3 <- read.csv("features_OGD_1_5h_slice_3_striatum_vid_3.csv")
feat_OGD_1_5h_slice_3_striatum_vid_4 <- read.csv("features_OGD_1_5h_slice_3_striatum_vid_4.csv")
feat_OGD_1_5h_slice_3_striatum_vid_5 <- read.csv("features_OGD_1_5h_slice_3_striatum_vid_5.csv")

feat_OGD_1_5h_slice_3_striatum_vid_1$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_3_striatum_vid_1$Slice <- "OGD S3"
feat_OGD_1_5h_slice_3_striatum_vid_1$Video <- "OGD V1"

feat_OGD_1_5h_slice_3_striatum_vid_2$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_3_striatum_vid_2$Slice <- "OGD S3"
feat_OGD_1_5h_slice_3_striatum_vid_2$Video <- "OGD V2"

feat_OGD_1_5h_slice_3_striatum_vid_3$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_3_striatum_vid_3$Slice <- "OGD S3"
feat_OGD_1_5h_slice_3_striatum_vid_3$Video <- "OGD V3"

feat_OGD_1_5h_slice_3_striatum_vid_4$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_3_striatum_vid_4$Slice <- "OGD S3"
feat_OGD_1_5h_slice_3_striatum_vid_4$Video <- "OGD V4"

feat_OGD_1_5h_slice_3_striatum_vid_5$Treatment <- "OGD 1.5h"
feat_OGD_1_5h_slice_3_striatum_vid_5$Slice <- "OGD S3"
feat_OGD_1_5h_slice_3_striatum_vid_5$Video <- "OGD V5"


##### bind all the files together
mpt <- bind_rows(feat_NT_slice_1_striatum_vid_1,
                 feat_NT_slice_1_striatum_vid_2,
                 feat_NT_slice_1_striatum_vid_3,
                 feat_NT_slice_1_striatum_vid_4,
                 feat_NT_slice_1_striatum_vid_5,
                 feat_NT_slice_2_striatum_vid_1,
                 feat_NT_slice_2_striatum_vid_2,
                 feat_NT_slice_2_striatum_vid_3,
                 feat_NT_slice_2_striatum_vid_4,
                 feat_NT_slice_2_striatum_vid_5,
                 feat_OGD_0_5h_slice_1_striatum_vid_1,
                 feat_OGD_0_5h_slice_1_striatum_vid_2,
                 feat_OGD_0_5h_slice_1_striatum_vid_3,
                 feat_OGD_0_5h_slice_1_striatum_vid_4,
                 feat_OGD_0_5h_slice_1_striatum_vid_5,
                 feat_OGD_0_5h_slice_2_striatum_vid_1,
                 feat_OGD_0_5h_slice_2_striatum_vid_2,
                 feat_OGD_0_5h_slice_2_striatum_vid_3,
                 feat_OGD_0_5h_slice_2_striatum_vid_4,
                 feat_OGD_0_5h_slice_2_striatum_vid_5,
                 feat_OGD_0_5h_slice_3_striatum_vid_1,
                 feat_OGD_0_5h_slice_3_striatum_vid_2,
                 feat_OGD_0_5h_slice_3_striatum_vid_3,
                 feat_OGD_0_5h_slice_3_striatum_vid_4,
                 feat_OGD_0_5h_slice_3_striatum_vid_5,
                 feat_OGD_1_5h_slice_1_striatum_vid_1,
                 feat_OGD_1_5h_slice_1_striatum_vid_2,
                 feat_OGD_1_5h_slice_1_striatum_vid_3,
                 feat_OGD_1_5h_slice_1_striatum_vid_4,
                 feat_OGD_1_5h_slice_1_striatum_vid_5,
                 feat_OGD_1_5h_slice_2_striatum_vid_1,
                 feat_OGD_1_5h_slice_2_striatum_vid_2,
                 feat_OGD_1_5h_slice_2_striatum_vid_3,
                 feat_OGD_1_5h_slice_2_striatum_vid_4,
                 feat_OGD_1_5h_slice_2_striatum_vid_5,
                 feat_OGD_1_5h_slice_3_striatum_vid_1,
                 feat_OGD_1_5h_slice_3_striatum_vid_2,
                 feat_OGD_1_5h_slice_3_striatum_vid_3,
                 feat_OGD_1_5h_slice_3_striatum_vid_4,
                 feat_OGD_1_5h_slice_3_striatum_vid_5)



mpt$Treatment <- as.factor(mpt$Treatment)

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
                          
                          'frames'
)]

print(features_for_corr)

no_nas = na.omit(features_for_corr)
cor_data = cor(no_nas)

corrplot(cor_data, method="circle", diag=FALSE, order='FPC', type='lower')

# m1 = lmer(Mean.deff1 ~ relevel(Treatment, ref="HC") + (1|Slice) , data=mpt)
# m0 = lmer(Mean.Deff1 ~ (1|Slice), data=mpt)
# anova(m0,m1)


plot_data = mpt[c('Mean.efficiency', 'Slice', 'Treatment', 'frames')]
plot_data <- na.omit(plot_data)
plot_data$Mean.efficiency <- plot_data$Mean.efficiency+1
ggplot(plot_data, aes(x=log(Mean.efficiency)))+
  geom_histogram()

clean_and_mean_by_treatment <- function(mpt, column_name) {
  mpt %>%
    group_by(Treatment) %>%
    filter(is.finite(.data[[column_name]])) %>%
    summarize(mean = mean(.data[[column_name]], na.rm = TRUE))
}

out = clean_and_mean_by_treatment(mpt, "Mean.fractal_dim")
out
confint(m1)

m1 = lmer(log(Mean.fractal_dim) ~ relevel(Treatment, ref="HC") + frames + (1|Slice), data=mpt)
m0 = lmer(log(Mean.efficiency) ~ frames + (1|Slice) + (1|frames), data=plot_data)

anova(m0,m1)
summary(m1)
confint(m1)

p <- ggplot(mpt, aes(x = Treatment, y = Mean.fractal_dim, fill = Treatment)) +
  geom_violin(alpha = 1, trim=TRUE, colour= NA, show.legend = FALSE) +
  
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               colour = "black")+
  coord_cartesian(ylim = c(1,5.5))+
  scale_y_continuous(breaks = seq(1, 5.5, by=0.5))+
  #theme_minimal()+
  labs(y="Mean fractal_dim")+
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
modifier <-0.25

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
  annotate("text", x = 1.5, y = base_value+1*modifier, label = "*", size = 6) +  # Add significance stars between 1 and 2
  # #   
  # #   # You can repeat this process for other comparisons
  # geom_segment(aes(x = 2.05, xend = 3, y = base_value, yend = base_value), size = 0.5) +  # Bar between Region 2 and 3
  # #   geom_segment(aes(x = 2, xend = 2, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 2
  # #   geom_segment(aes(x = 3, xend = 3, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 3
  # # #   
  # annotate("text", x = 2.5, y = base_value+1*modifier, label = "***", size = 6) + # Add significance stars between 2 and 3
  # # 
  # # # You can repeat this process for other comparisons
  geom_segment(aes(x = 1, xend = 3, y = base_value+2*modifier, yend = base_value+2*modifier), size = 0.5) +  # Bar between Region 2 and 3
  #   geom_segment(aes(x = 1, xend = 1, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 2
  #   geom_segment(aes(x = 3, xend = 3, y = 0.25, yend = 0.23), size = 0.5) +  # Vertical tick for Region 3
  # #   
  annotate("text", x = 2, y = base_value+3*modifier, label = "*", size = 6)  # Add significance stars between 2 and 3

