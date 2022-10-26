
# Violin plot

# Load the required packages
library(foreign)
library(tidyverse)
library(reshape2)
library(svglite)

# Set seed for replicability
set.seed(2022)

# Set the working directory to where the data is
setwd("")

# Read in the data (in a wide format)
Violin_i <- read.spss("262400278_v1.sav")

# Convert to a data frame
Violin_p <- data.frame(Violin_i)

# Create a sharing count variable
Violin_p$Shares_Count <- Violin_p$Share_dummy_1+ Violin_p$Share_dummy_2+ Violin_p$Share_dummy_3+
  Violin_p$Share_dummy_4+ Violin_p$Share_dummy_5+ Violin_p$Share_dummy_6+
  Violin_p$Share_dummy_7+ Violin_p$Share_dummy_8+ Violin_p$Share_dummy_9+
  Violin_p$Share_dummy_10+ Violin_p$Share_dummy_11+ Violin_p$Share_dummy_12+
  Violin_p$Share_dummy_13+ Violin_p$Share_dummy_14+ Violin_p$Share_dummy_15

# Rename the relevant variables
Violin_p$Sharing <- Violin_p$Shares_Count
Violin_p$Reacting <- Violin_p$LCS_total
Violin_p$Liking <- Violin_p$Primary_Outcome_Count
names(Violin_p)[names(Violin_p) == 'Liking'] <- "Liking/Loving"

# Create a data frame with counts of "Liking/Loving", "Reacting", and "Sharing" 
# in the long format.
Violin_p_1 <- melt(Violin_p,id=c("uuid"), measure.vars = 
                     c("Liking/Loving", "Reacting", "Sharing"))

# Sanity check if the process worked
colnames(Violin_p_1)

# Rename the variables 
Violin_p_2 <- Violin_p_1 %>%
  rename(
    Accuracy = value, 
    Outcome = variable,
  )


# Set the working directory to where you want to save the plot
# setwd("")

# Create the violin plot object
Violin_secondary <- ggplot(Violin_p_2, aes(x=Outcome, y=Accuracy, fill=Outcome)) + 
  geom_violin(trim=FALSE) +
  stat_summary(fun="mean", geom="point", size=3) +
  scale_y_continuous(limits=c(-1,15), breaks=seq(0,15,3)) +
  labs(title="",x="Outcome", y = "Count", face="bold") 

# svglite("Violin_secondary_1.svg", width=14, height=6)

# Generate the Violin_secondary plot
Violin_secondary + theme_minimal() + theme(legend.position="none") + 
  theme(axis.text.x = element_text(size=16,  color = "black"),
        axis.text.y = element_text(size=16, color = "black")) +
  theme(axis.title = element_text(size=20, face="bold")) + 
  theme(axis.title.x = element_text(vjust=-0.5)) 


# dev.off()