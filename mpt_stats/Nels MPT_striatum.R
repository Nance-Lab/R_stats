library(lme4)
library(lmerTest)
library(tidyverse)
library(emmeans)
library(corrplot)
#setwd("Users/nelsschimek/Documents/nancelab/Data/OGD_severity/") ### Change this to whatever folder you save things to

##### Read in all the data and subset down to D_fit and D_eff1, then add variables for Age, Slice, and Video
##### NT


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

m1 = lmer((Mean.fractal_dim) ~ Treatment + frames + (1|Slice), data=mpt) #Assumes Age is a 3-level factor with levels P14, P35, and P70
summary(m1)
emm <- emmeans(m1, ~ Treatment)
age_contrast <- contrast(emm, method = list("str_vs_hippo" = c(0, 1, -1))) #Sets the values of the levels to 0,  1, and -1 therfore giving you the difference between level 2 (P35) and level 3 (P70).
age_contrast
confint(age_contrast)
emm

# Compute means for each group
means <- mpt %>%
  group_by(Treatment) %>%
  summarise(Mean = mean(Mean.Deff1))

# Calculate fold differences compared to control
control_mean <- means %>%
  filter(Treatment == "HC") %>%
  pull(Mean)

fold_differences <- means %>%
  mutate(Fold_Difference = Mean / control_mean)

print(fold_differences)


df <- mpt
df[sapply(df, is.infinite)] <- NA



# Perform Kruskal-Wallis test
kruskal_test <- kruskal.test(Mean.Deff1 ~ Treatment, data = df)
print(kruskal_test)


pairwise_test <- pairwise.wilcox.test(df$Mean.Deff1, df$Treatment, p.adjust.method = "bonferroni")
print(pairwise_test)

# install.packages("ggplot2")
# install.packages("ggbeeswarm")
library(ggplot2)
# library(ggbeeswarm)
# 

#   

clean_and_mean_by_treatment <- function(df, column_name) {
  df %>%
    group_by(Treatment) %>%
    filter(is.finite(.data[[column_name]])) %>%
    summarize(mean = mean(.data[[column_name]], na.rm = TRUE))
}

out = clean_and_mean_by_treatment(mpt, "Mean.")
out

p <- ggplot(mpt, aes(x = Treatment, y = Mean.Deff1, fill = Treatment)) +
  geom_violin(alpha = 1, trim=TRUE, colour= NA) +
  
  stat_summary(fun = "mean",
               geom = "crossbar",
               width = 0.1,
               colour = "black")+
  coord_cartesian(ylim = c(0,10))+
  scale_y_continuous(breaks = seq(0, 10, by=1))+
  #theme_minimal()+
  labs(y="Mean Deff1")+
  theme(
    text = element_text(family="Arial"),
    axis.title.y = element_text(size=18, face="bold", family="Arial"),
    axis.title.x = element_text(size=18, face="bold", family="Arial"),
    axis.text.x = element_text(size=18, face="bold", family="Arial"),
    legend.text = element_text(size=18, face="bold", family="Arial"),
    axis.text.y = element_text(size=18, face="bold", family="Arial"),
    panel.background = element_rect(fill = "white", colour = "black"),
    plot.background = element_rect(fill = "white", colour = NA),
    #axis.line = element_line(colour = "black")
  ) +
  guides(fill=FALSE)

p +
  #annotate("text", x=2.0, y=3, label = "*", color="black", size=10, fontface="bold")+
  annotate("text", x=3.0, y=10, label = "*", color="black", size=10, fontface="bold")


  


