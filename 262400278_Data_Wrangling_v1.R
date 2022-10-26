

# Load the required packages
library(reshape2)
library(tidyverse)

# Set seed for replicability
set.seed(2022)

# Set the working directory to where the data is
setwd("")

# Read in the data
data <- read.csv("262400278_v1.csv")

# Select the relevant columns (shares)
data1 <- data[,c("uuid", "ARM", "share_buttons_1r1c1", "share_buttons_2r1c1", "share_buttons_3r1c1",
                 "share_buttons_4r1c1", "share_buttons_5r1c1", "share_buttons_6r1c1", "share_buttons_7r1c1",
                 "share_buttons_8r1c1", "share_buttons_9r1c1", "share_buttons_10r1c1", "share_buttons_11r1c1",
                 "share_buttons_12r1c1", "share_buttons_13r1c1", "share_buttons_14r1c1", "share_buttons_15r1c1",
                 "share_buttons_16r1c1", "share_buttons_17r1c1", "share_buttons_18r1c1", "share_buttons_19r1c1",
                 "share_buttons_20r1c1", "share_buttons_21r1c1", "share_buttons_22r1c1", "share_buttons_23r1c1",
                 "share_buttons_24r1c1", "share_buttons_25r1c1", "share_buttons_26r1c1", "share_buttons_27r1c1",
                 "share_buttons_28r1c1", "share_buttons_29r1c1", "share_buttons_30r1c1",
                 
                 "share_buttons_1r2c1", "share_buttons_2r2c1",
                 "share_buttons_3r2c1", "share_buttons_4r2c1", "share_buttons_5r2c1", "share_buttons_6r2c1", 
                 "share_buttons_7r2c1", "share_buttons_8r2c1", "share_buttons_9r2c1", "share_buttons_10r2c1", 
                 "share_buttons_11r2c1", "share_buttons_12r2c1", "share_buttons_13r2c1", "share_buttons_14r2c1", 
                 "share_buttons_15r2c1", "share_buttons_16r2c1", "share_buttons_17r2c1", "share_buttons_18r2c1", 
                 "share_buttons_19r2c1", "share_buttons_20r2c1", "share_buttons_21r2c1", "share_buttons_22r2c1", 
                 "share_buttons_23r2c1", "share_buttons_24r2c1", "share_buttons_25r2c1", "share_buttons_26r2c1", 
                 "share_buttons_27r2c1", "share_buttons_28r2c1", "share_buttons_29r2c1", "share_buttons_30r2c1",
                 
                 "share_buttons_1r3c1", "share_buttons_2r3c1",
                 "share_buttons_3r3c1", "share_buttons_4r3c1", "share_buttons_5r3c1", "share_buttons_6r3c1", 
                 "share_buttons_7r3c1", "share_buttons_8r3c1", "share_buttons_9r3c1", "share_buttons_10r3c1", 
                 "share_buttons_11r3c1", "share_buttons_12r3c1", "share_buttons_13r3c1", "share_buttons_14r3c1", 
                 "share_buttons_15r3c1", "share_buttons_16r3c1", "share_buttons_17r3c1", "share_buttons_18r3c1", 
                 "share_buttons_19r3c1", "share_buttons_20r3c1", "share_buttons_21r3c1", "share_buttons_22r3c1", 
                 "share_buttons_23r3c1", "share_buttons_24r3c1", "share_buttons_25r3c1", "share_buttons_26r3c1", 
                 "share_buttons_27r3c1", "share_buttons_28r3c1", "share_buttons_29r3c1", "share_buttons_30r3c1",
                 
                 "share_buttons_1r4c1", "share_buttons_2r4c1",
                 "share_buttons_3r4c1", "share_buttons_4r4c1", "share_buttons_5r4c1", "share_buttons_6r4c1", 
                 "share_buttons_7r4c1", "share_buttons_8r4c1", "share_buttons_9r4c1", "share_buttons_10r4c1", 
                 "share_buttons_11r4c1", "share_buttons_12r4c1", "share_buttons_13r4c1", "share_buttons_14r4c1", 
                 "share_buttons_15r4c1", "share_buttons_16r4c1", "share_buttons_17r4c1", "share_buttons_18r4c1", 
                 "share_buttons_19r4c1", "share_buttons_20r4c1", "share_buttons_21r4c1", "share_buttons_22r4c1", 
                 "share_buttons_23r4c1", "share_buttons_24r4c1", "share_buttons_25r4c1", "share_buttons_26r4c1", 
                 "share_buttons_27r4c1", "share_buttons_28r4c1", "share_buttons_29r4c1", "share_buttons_30r4c1",
                 
                 "share_buttons_1r5c1", "share_buttons_2r5c1",
                 "share_buttons_3r5c1", "share_buttons_4r5c1", "share_buttons_5r5c1", "share_buttons_6r5c1", 
                 "share_buttons_7r5c1", "share_buttons_8r5c1", "share_buttons_9r5c1", "share_buttons_10r5c1", 
                 "share_buttons_11r5c1", "share_buttons_12r5c1", "share_buttons_13r5c1", "share_buttons_14r5c1", 
                 "share_buttons_15r5c1", "share_buttons_16r5c1", "share_buttons_17r5c1", "share_buttons_18r5c1", 
                 "share_buttons_19r5c1", "share_buttons_20r5c1", "share_buttons_21r5c1", "share_buttons_22r5c1", 
                 "share_buttons_23r5c1", "share_buttons_24r5c1", "share_buttons_25r5c1", "share_buttons_26r5c1", 
                 "share_buttons_27r5c1", "share_buttons_28r5c1", "share_buttons_29r5c1", "share_buttons_30r5c1")]

# Convert into long data frame (shares)
data2 <- melt(data1,id=c("uuid", "ARM"), measure.vars = 
                c("share_buttons_1r1c1", "share_buttons_2r1c1", "share_buttons_3r1c1",
                  "share_buttons_4r1c1", "share_buttons_5r1c1", "share_buttons_6r1c1", "share_buttons_7r1c1",
                  "share_buttons_8r1c1", "share_buttons_9r1c1", "share_buttons_10r1c1", "share_buttons_11r1c1",
                  "share_buttons_12r1c1", "share_buttons_13r1c1", "share_buttons_14r1c1", "share_buttons_15r1c1",
                  "share_buttons_16r1c1", "share_buttons_17r1c1", "share_buttons_18r1c1", "share_buttons_19r1c1",
                  "share_buttons_20r1c1", "share_buttons_21r1c1", "share_buttons_22r1c1", "share_buttons_23r1c1",
                  "share_buttons_24r1c1", "share_buttons_25r1c1", "share_buttons_26r1c1", "share_buttons_27r1c1",
                  "share_buttons_28r1c1", "share_buttons_29r1c1", "share_buttons_30r1c1"))

# Rename variables
data2i <- data2 %>%
  rename(
    Accuracy_Share_Friends = value, 
    Share_Friends = variable,
  )

# Create and ID variable for melting
data2ii <- tibble::rowid_to_column(data2i, "ID")

# Repeat the process for different types of shares
data3 <- melt(data1,id=c("uuid", "ARM"), measure.vars = 
                c("share_buttons_1r2c1", "share_buttons_2r2c1",
                  "share_buttons_3r2c1", "share_buttons_4r2c1", "share_buttons_5r2c1", "share_buttons_6r2c1", 
                  "share_buttons_7r2c1", "share_buttons_8r2c1", "share_buttons_9r2c1", "share_buttons_10r2c1", 
                  "share_buttons_11r2c1", "share_buttons_12r2c1", "share_buttons_13r2c1", "share_buttons_14r2c1", 
                  "share_buttons_15r2c1", "share_buttons_16r2c1", "share_buttons_17r2c1", "share_buttons_18r2c1", 
                  "share_buttons_19r2c1", "share_buttons_20r2c1", "share_buttons_21r2c1", "share_buttons_22r2c1", 
                  "share_buttons_23r2c1", "share_buttons_24r2c1", "share_buttons_25r2c1", "share_buttons_26r2c1", 
                  "share_buttons_27r2c1", "share_buttons_28r2c1", "share_buttons_29r2c1", "share_buttons_30r2c1"))
data3i <- data3 %>%
  rename(
    Accuracy_Share_Feed = value, 
    Share_Feed = variable,
  )

data3ii <- tibble::rowid_to_column(data3i, "ID")

data4 <- melt(data1,id=c("uuid", "ARM"), measure.vars = 
                c("share_buttons_1r3c1", "share_buttons_2r3c1",
                  "share_buttons_3r3c1", "share_buttons_4r3c1", "share_buttons_5r3c1", "share_buttons_6r3c1", 
                  "share_buttons_7r3c1", "share_buttons_8r3c1", "share_buttons_9r3c1", "share_buttons_10r3c1", 
                  "share_buttons_11r3c1", "share_buttons_12r3c1", "share_buttons_13r3c1", "share_buttons_14r3c1", 
                  "share_buttons_15r3c1", "share_buttons_16r3c1", "share_buttons_17r3c1", "share_buttons_18r3c1", 
                  "share_buttons_19r3c1", "share_buttons_20r3c1", "share_buttons_21r3c1", "share_buttons_22r3c1", 
                  "share_buttons_23r3c1", "share_buttons_24r3c1", "share_buttons_25r3c1", "share_buttons_26r3c1", 
                  "share_buttons_27r3c1", "share_buttons_28r3c1", "share_buttons_29r3c1", "share_buttons_30r3c1"))

data4i <- data4 %>%
  rename(
    Accuracy_Share_Messenger = value, 
    Share_Messenger = variable,
  )

data4ii <- tibble::rowid_to_column(data4i, "ID")

data5 <- melt(data1,id=c("uuid", "ARM"), measure.vars = 
                c(                 "share_buttons_1r4c1", "share_buttons_2r4c1",
                                   "share_buttons_3r4c1", "share_buttons_4r4c1", "share_buttons_5r4c1", "share_buttons_6r4c1", 
                                   "share_buttons_7r4c1", "share_buttons_8r4c1", "share_buttons_9r4c1", "share_buttons_10r4c1", 
                                   "share_buttons_11r4c1", "share_buttons_12r4c1", "share_buttons_13r4c1", "share_buttons_14r4c1", 
                                   "share_buttons_15r4c1", "share_buttons_16r4c1", "share_buttons_17r4c1", "share_buttons_18r4c1", 
                                   "share_buttons_19r4c1", "share_buttons_20r4c1", "share_buttons_21r4c1", "share_buttons_22r4c1", 
                                   "share_buttons_23r4c1", "share_buttons_24r4c1", "share_buttons_25r4c1", "share_buttons_26r4c1", 
                                   "share_buttons_27r4c1", "share_buttons_28r4c1", "share_buttons_29r4c1", "share_buttons_30r4c1"))

data5i <- data5 %>%
  rename(
    Accuracy_Share_Group = value, 
    Share_Group = variable,
  )

data5ii <- tibble::rowid_to_column(data5i, "ID")

data6 <- melt(data1,id=c("uuid", "ARM"), measure.vars = 
                c(                 "share_buttons_1r5c1", "share_buttons_2r5c1",
                                   "share_buttons_3r5c1", "share_buttons_4r5c1", "share_buttons_5r5c1", "share_buttons_6r5c1", 
                                   "share_buttons_7r5c1", "share_buttons_8r5c1", "share_buttons_9r5c1", "share_buttons_10r5c1", 
                                   "share_buttons_11r5c1", "share_buttons_12r5c1", "share_buttons_13r5c1", "share_buttons_14r5c1", 
                                   "share_buttons_15r5c1", "share_buttons_16r5c1", "share_buttons_17r5c1", "share_buttons_18r5c1", 
                                   "share_buttons_19r5c1", "share_buttons_20r5c1", "share_buttons_21r5c1", "share_buttons_22r5c1", 
                                   "share_buttons_23r5c1", "share_buttons_24r5c1", "share_buttons_25r5c1", "share_buttons_26r5c1", 
                                   "share_buttons_27r5c1", "share_buttons_28r5c1", "share_buttons_29r5c1", "share_buttons_30r5c1"))

data6i <- data6 %>%
  rename(
    Accuracy_Share_Profile = value, 
    Share_Profile = variable,
  )


data6ii <- tibble::rowid_to_column(data6i, "ID")

# Merge different types of shares into one data frame, by ID
data7 <- merge(data2ii, data3ii, by = "ID")

data8 <- merge(data7, data4ii, by = "ID")

data9 <- merge(data8, data5ii, by = "ID")

data10 <- merge(data9, data6ii, by = "ID")

# Get rid of redundant columns
data11 <- subset(data10, select = c(-ARM.x, -ARM.y, -uuid.x, -uuid.y, -Share_Friends, 
                                    -Share_Feed, -Share_Messenger, -Share_Group, -Share_Profile))
data12 <- subset(data11, select = c(-ARM.x, -ARM.y, -uuid.x, -uuid.y))

colnames(data12)

# Sum different shares
data12$Shares <-rowSums(cbind(data12$Accuracy_Share_Friends, data12$Accuracy_Share_Feed,
                              data12$Accuracy_Share_Messenger, data12$Accuracy_Share_Group,
                              data12$Accuracy_Share_Profile), na.rm=TRUE) 

# Assign a value of 1 to cells with a count of shares higher than 1
data12$Share_Accuracy[data12$Shares > 0] <- 1

# Assign a value of 1 to cells with a count of shares lower than 1
data12$Share_Accuracy[data12$Shares < 1] <- 0

# Get rid of redundant columns
data13 <- subset(data12, select = c(-Accuracy_Share_Friends, -Accuracy_Share_Feed, 
                                    -Accuracy_Share_Messenger, -Accuracy_Share_Group,
                                    -Accuracy_Share_Profile))

colnames(data13)

# Repeat the process for all reactions
data14<-data[,c("uuid", "ARM", "lcs_buttons_1r1c1", "lcs_buttons_2r1c1", "lcs_buttons_3r1c1",
                "lcs_buttons_4r1c1", "lcs_buttons_5r1c1", "lcs_buttons_6r1c1", "lcs_buttons_7r1c1",
                "lcs_buttons_8r1c1", "lcs_buttons_9r1c1", "lcs_buttons_10r1c1", "lcs_buttons_11r1c1",
                "lcs_buttons_12r1c1", "lcs_buttons_13r1c1", "lcs_buttons_14r1c1", "lcs_buttons_15r1c1",
                "lcs_buttons_16r1c1", "lcs_buttons_17r1c1", "lcs_buttons_18r1c1", "lcs_buttons_19r1c1",
                "lcs_buttons_20r1c1", "lcs_buttons_21r1c1", "lcs_buttons_22r1c1", "lcs_buttons_23r1c1",
                "lcs_buttons_24r1c1", "lcs_buttons_25r1c1", "lcs_buttons_26r1c1", "lcs_buttons_27r1c1",
                "lcs_buttons_28r1c1", "lcs_buttons_29r1c1", "lcs_buttons_30r1c1",
                
                "emoji_buttons_1r1c1", "emoji_buttons_2r1c1", "emoji_buttons_3r1c1", "emoji_buttons_4r1c1",
                "emoji_buttons_5r1c1", "emoji_buttons_6r1c1", "emoji_buttons_7r1c1", "emoji_buttons_8r1c1",
                "emoji_buttons_9r1c1", "emoji_buttons_10r1c1", "emoji_buttons_11r1c1", "emoji_buttons_12r1c1",
                "emoji_buttons_13r1c1", "emoji_buttons_14r1c1", "emoji_buttons_15r1c1", "emoji_buttons_16r1c1",
                "emoji_buttons_17r1c1", "emoji_buttons_18r1c1", "emoji_buttons_19r1c1", "emoji_buttons_20r1c1",
                "emoji_buttons_21r1c1", "emoji_buttons_22r1c1", "emoji_buttons_23r1c1", "emoji_buttons_24r1c1",
                "emoji_buttons_25r1c1", "emoji_buttons_26r1c1", "emoji_buttons_27r1c1", "emoji_buttons_28r1c1",
                "emoji_buttons_29r1c1", "emoji_buttons_30r1c1",
                
                "emoji_buttons_1r2c1", "emoji_buttons_2r2c1", "emoji_buttons_3r2c1", "emoji_buttons_4r2c1",
                "emoji_buttons_5r2c1", "emoji_buttons_6r2c1", "emoji_buttons_7r2c1", "emoji_buttons_8r2c1",
                "emoji_buttons_9r2c1", "emoji_buttons_10r2c1", "emoji_buttons_11r2c1", "emoji_buttons_12r2c1",
                "emoji_buttons_13r2c1", "emoji_buttons_14r2c1", "emoji_buttons_15r2c1", "emoji_buttons_16r2c1",
                "emoji_buttons_17r2c1", "emoji_buttons_18r2c1", "emoji_buttons_19r2c1", "emoji_buttons_20r2c1",
                "emoji_buttons_21r2c1", "emoji_buttons_22r2c1", "emoji_buttons_23r2c1", "emoji_buttons_24r2c1",
                "emoji_buttons_25r2c1", "emoji_buttons_26r2c1", "emoji_buttons_27r2c1", "emoji_buttons_28r2c1",
                "emoji_buttons_29r2c1", "emoji_buttons_30r2c1")]



data15 <- melt(data14,id=c("uuid", "ARM"), measure.vars = 
                 c("lcs_buttons_1r1c1", "lcs_buttons_2r1c1", "lcs_buttons_3r1c1",
                   "lcs_buttons_4r1c1", "lcs_buttons_5r1c1", "lcs_buttons_6r1c1", "lcs_buttons_7r1c1",
                   "lcs_buttons_8r1c1", "lcs_buttons_9r1c1", "lcs_buttons_10r1c1", "lcs_buttons_11r1c1",
                   "lcs_buttons_12r1c1", "lcs_buttons_13r1c1", "lcs_buttons_14r1c1", "lcs_buttons_15r1c1",
                   "lcs_buttons_16r1c1", "lcs_buttons_17r1c1", "lcs_buttons_18r1c1", "lcs_buttons_19r1c1",
                   "lcs_buttons_20r1c1", "lcs_buttons_21r1c1", "lcs_buttons_22r1c1", "lcs_buttons_23r1c1",
                   "lcs_buttons_24r1c1", "lcs_buttons_25r1c1", "lcs_buttons_26r1c1", "lcs_buttons_27r1c1",
                   "lcs_buttons_28r1c1", "lcs_buttons_29r1c1", "lcs_buttons_30r1c1"))

data15i <- data15 %>%
  rename(
    Accuracy_Reactions = value, 
    Redundant_Variable_R = variable,
  )

data15ii <- tibble::rowid_to_column(data15i, "ID")


data16 <- melt(data14,id=c("uuid", "ARM"), measure.vars = 
                 c("emoji_buttons_1r1c1", "emoji_buttons_2r1c1", "emoji_buttons_3r1c1", "emoji_buttons_4r1c1",
                   "emoji_buttons_5r1c1", "emoji_buttons_6r1c1", "emoji_buttons_7r1c1", "emoji_buttons_8r1c1",
                   "emoji_buttons_9r1c1", "emoji_buttons_10r1c1", "emoji_buttons_11r1c1", "emoji_buttons_12r1c1",
                   "emoji_buttons_13r1c1", "emoji_buttons_14r1c1", "emoji_buttons_15r1c1", "emoji_buttons_16r1c1",
                   "emoji_buttons_17r1c1", "emoji_buttons_18r1c1", "emoji_buttons_19r1c1", "emoji_buttons_20r1c1",
                   "emoji_buttons_21r1c1", "emoji_buttons_22r1c1", "emoji_buttons_23r1c1", "emoji_buttons_24r1c1",
                   "emoji_buttons_25r1c1", "emoji_buttons_26r1c1", "emoji_buttons_27r1c1", "emoji_buttons_28r1c1",
                   "emoji_buttons_29r1c1", "emoji_buttons_30r1c1"))

data16i <- data16 %>%
  rename(
    Accuracy_Likes = value, 
    Redundant_Variable_Li = variable,
  )

data16ii <- tibble::rowid_to_column(data16i, "ID")

data17 <- melt(data14,id=c("uuid", "ARM"), measure.vars = 
                 c("emoji_buttons_1r2c1", "emoji_buttons_2r2c1", "emoji_buttons_3r2c1", "emoji_buttons_4r2c1",
                   "emoji_buttons_5r2c1", "emoji_buttons_6r2c1", "emoji_buttons_7r2c1", "emoji_buttons_8r2c1",
                   "emoji_buttons_9r2c1", "emoji_buttons_10r2c1", "emoji_buttons_11r2c1", "emoji_buttons_12r2c1",
                   "emoji_buttons_13r2c1", "emoji_buttons_14r2c1", "emoji_buttons_15r2c1", "emoji_buttons_16r2c1",
                   "emoji_buttons_17r2c1", "emoji_buttons_18r2c1", "emoji_buttons_19r2c1", "emoji_buttons_20r2c1",
                   "emoji_buttons_21r2c1", "emoji_buttons_22r2c1", "emoji_buttons_23r2c1", "emoji_buttons_24r2c1",
                   "emoji_buttons_25r2c1", "emoji_buttons_26r2c1", "emoji_buttons_27r2c1", "emoji_buttons_28r2c1",
                   "emoji_buttons_29r2c1", "emoji_buttons_30r2c1"))


data17i <- data17 %>%
  rename(
    Accuracy_Love = value, 
    Redundant_Variable_Lo = variable,
  )


data17ii <- tibble::rowid_to_column(data17i, "ID")

data18 <- merge(data15ii, data16ii, by = "ID")

data19 <- merge(data18, data17ii, by = "ID")

data20 <- subset(data19, select = c(-Redundant_Variable_Li, -Redundant_Variable_Lo,
                                    -ARM.x, -ARM.y, -uuid.x, -uuid.y))

data21 <- merge(data20, data13, by = "ID", all.x = TRUE)

nrow(data21)
# 72900

identical(data21[['uuid.x']],data21[['uuid.y']])
# TRUE

# The two columns are the same

data22 <- subset(data21, select = c(-ARM.y, -uuid.y))

data22$Primary <-rowSums(cbind(data22$Accuracy_Likes, data22$Accuracy_Love), na.rm=TRUE) 

range(data22$Primary)


colnames(data22)[which(names(data22) == "uuid.x")] <- "uuid"

colnames(data22)[which(names(data22) == "ARM.x")] <- "ARM"

# Create a function to identify each post
Post_Function <- function(PF) {
  if (str_detect(PF, "_1r")==TRUE) {
    Post_ID <- "1"
  } else {
    if (str_detect(PF, "_2r")==TRUE) {
      Post_ID <- "2"
    } else {
      if (str_detect(PF, "_3r")==TRUE) {
        Post_ID <- "3"
      } else {
        if (str_detect(PF, "_4r")==TRUE) {
          Post_ID <- "4"
        } else {
          if (str_detect(PF, "_5r")==TRUE) {
            Post_ID <- "5"
          } else {
            if (str_detect(PF, "_6r")==TRUE) {
              Post_ID <- "6"
            } else {
              if (str_detect(PF, "_7r")==TRUE) {
                Post_ID <- "7"
              } else {
                if (str_detect(PF, "_8r")==TRUE) {
                  Post_ID <- "8"
                } else {
                  if (str_detect(PF, "_9r")==TRUE) {
                    Post_ID <- "9"
                  } else {
                    if (str_detect(PF, "_10")==TRUE) {
                      Post_ID <- "10"
                    } else {
                      if (str_detect(PF, "_11")==TRUE) {
                        Post_ID <- "11"
                      } else {
                        if (str_detect(PF, "_12")==TRUE) {
                          Post_ID <- "12"
                        } else {
                          if (str_detect(PF, "_13")==TRUE) {
                            Post_ID <- "13"
                          } else {
                            if (str_detect(PF, "_14")==TRUE) {
                              Post_ID <- "14"
                            } else {
                              if (str_detect(PF, "_15")==TRUE) {
                                Post_ID <- "15"
                              } else {
                                if (str_detect(PF, "_16")==TRUE) {
                                  Post_ID <- "16"
                                } else {
                                  if (str_detect(PF, "_17")==TRUE) {
                                    Post_ID <- "17"
                                  } else {
                                    if (str_detect(PF, "_18")==TRUE) {
                                      Post_ID <- "18"
                                    } else {
                                      if (str_detect(PF, "_19")==TRUE) {
                                        Post_ID <- "19"
                                      } else {
                                        if (str_detect(PF, "_20")==TRUE) {
                                          Post_ID <- "20"
                                        } else {
                                          if (str_detect(PF, "_21")==TRUE) {
                                            Post_ID <- "21"
                                          } else {
                                            if (str_detect(PF, "_22")==TRUE) {
                                              Post_ID <- "22"
                                            } else {
                                              if (str_detect(PF, "_23")==TRUE) {
                                                Post_ID <- "23"
                                              } else {
                                                if (str_detect(PF, "_24")==TRUE) {
                                                  Post_ID <- "24"
                                                } else {
                                                  if (str_detect(PF, "_25")==TRUE) {
                                                    Post_ID <- "25"
                                                  } else {
                                                    if (str_detect(PF, "_26")==TRUE) {
                                                      Post_ID <- "26"
                                                    } else {
                                                      if (str_detect(PF, "_27")==TRUE) {
                                                        Post_ID <- "27"
                                                      } else {
                                                        if (str_detect(PF, "_28")==TRUE) {
                                                          Post_ID <- "28"
                                                        } else {
                                                          if (str_detect(PF, "_29")==TRUE) {
                                                            Post_ID <- "29"
                                                          } else {
                                                            if (str_detect(PF, "_30")==TRUE) {
                                                              Post_ID <- "30"
                                                            }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}

data23 <- data22

# Create a new variable Post
data23$Post <- NA

# Run a loop to assign post id to the variable Post
for (i in 1:nrow(data23)) {
  data23$Post[i] <- Post_Function(PF = data23$Redundant_Variable_R[i])
}

# Create a function to identify whether each post is misinformation or not
Misinfo_Function <- function(MF) {
  if (str_detect(MF, "_1r")==TRUE) {
    Post_ID <- "1"
  } else {
    if (str_detect(MF, "_2r")==TRUE) {
      Post_ID <- "1"
    } else {
      if (str_detect(MF, "_3r")==TRUE) {
        Post_ID <- "1"
      } else {
        if (str_detect(MF, "_4r")==TRUE) {
          Post_ID <- "1"
        } else {
          if (str_detect(MF, "_5r")==TRUE) {
            Post_ID <- "1"
          } else {
            if (str_detect(MF, "_6r")==TRUE) {
              Post_ID <- "1"
            } else {
              if (str_detect(MF, "_7r")==TRUE) {
                Post_ID <- "1"
              } else {
                if (str_detect(MF, "_8r")==TRUE) {
                  Post_ID <- "1"
                } else {
                  if (str_detect(MF, "_9r")==TRUE) {
                    Post_ID <- "1"
                  } else {
                    if (str_detect(MF, "_10")==TRUE) {
                      Post_ID <- "1"
                    } else {
                      if (str_detect(MF, "_11")==TRUE) {
                        Post_ID <- "1"
                      } else {
                        if (str_detect(MF, "_12")==TRUE) {
                          Post_ID <- "1"
                        } else {
                          if (str_detect(MF, "_13")==TRUE) {
                            Post_ID <- "1"
                          } else {
                            if (str_detect(MF, "_14")==TRUE) {
                              Post_ID <- "1"
                            } else {
                              if (str_detect(MF, "_15")==TRUE) {
                                Post_ID <- "1"
                              } else {
                                if (str_detect(MF, "_16")==TRUE) {
                                  Post_ID <- "0"
                                } else {
                                  if (str_detect(MF, "_17")==TRUE) {
                                    Post_ID <- "0"
                                  } else {
                                    if (str_detect(MF, "_18")==TRUE) {
                                      Post_ID <- "0"
                                    } else {
                                      if (str_detect(MF, "_19")==TRUE) {
                                        Post_ID <- "0"
                                      } else {
                                        if (str_detect(MF, "_20")==TRUE) {
                                          Post_ID <- "0"
                                        } else {
                                          if (str_detect(MF, "_21")==TRUE) {
                                            Post_ID <- "0"
                                          } else {
                                            if (str_detect(MF, "_22")==TRUE) {
                                              Post_ID <- "0"
                                            } else {
                                              if (str_detect(MF, "_23")==TRUE) {
                                                Post_ID <- "0"
                                              } else {
                                                if (str_detect(MF, "_24")==TRUE) {
                                                  Post_ID <- "0"
                                                } else {
                                                  if (str_detect(MF, "_25")==TRUE) {
                                                    Post_ID <- "0"
                                                  } else {
                                                    if (str_detect(MF, "_26")==TRUE) {
                                                      Post_ID <- "0"
                                                    } else {
                                                      if (str_detect(MF, "_27")==TRUE) {
                                                        Post_ID <- "0"
                                                      } else {
                                                        if (str_detect(MF, "_28")==TRUE) {
                                                          Post_ID <- "0"
                                                        } else {
                                                          if (str_detect(MF, "_29")==TRUE) {
                                                            Post_ID <- "0"
                                                          } else {
                                                            if (str_detect(MF, "_30")==TRUE) {
                                                              Post_ID <- "0"
                                                            }}}}}}}}}}}}}}}}}}}}}}}}}}}}}}}



data24 <- data23

# Create a new variable Post
data24$Misinfo <- NA

# Run a loop to assign misinformation value to each post (1 or 0)
for (i in 1:nrow(data24)) {
  data24$Misinfo[i] <- Misinfo_Function(MF = data24$Redundant_Variable_R[i])
}



data25 <- subset(data24, select = -Redundant_Variable_R)


## Calculate CRT Scores
data_crt_1 <- data[,c("uuid", "ARM", "C1a", "C1b", "C1c",
                      "C1d", "C1e", "C1f", "C1g")]

# Setting NAs in CRT columns 999
data_crt_1[is.na(data_crt_1)] <- 999


# Scoring CRT

CRT_1a_Function <- function(CRT_1a) {
  
  if (str_detect(CRT_1a, "5")==TRUE) {
    
    Data_allocation <- "1"
    
  } else {
    
    if (str_detect(CRT_1a, "0.05")==TRUE) {
      
      Data_allocation <- "1"
      
    } else {
      
      if (str_detect(CRT_1a, ".05")==TRUE) {
        
        Data_allocation <- "1"
        
      } else{
        Data_allocation <- "0"
      }
    }
  }
}

CRT_1b_Function <- function(CRT_1b) {
  
  if (str_detect(CRT_1b, "5")==TRUE) {
    
    Data_allocation <- "1"
    
  } else{
    Data_allocation <- "0"
  }
}


CRT_1c_Function <- function(CRT_1c) {
  
  if (str_detect(CRT_1c, "47")==TRUE) {
    
    Data_allocation <- "1"
    
  } else{
    Data_allocation <- "0"
  }
}

CRT_1d_Function <- function(CRT_1d) {
  
  if (str_detect(CRT_1d, "2")==TRUE) {
    
    Data_allocation <- "1"
    
  } else{
    Data_allocation <- "0"
  }
}

CRT_1e_Function <- function(CRT_1e) {
  
  if (str_detect(CRT_1e, "8")==TRUE) {
    
    Data_allocation <- "1"
    
  } else{
    Data_allocation <- "0"
  }
}

CRT_1f_Function <- function(CRT_1f) {
  
  if (str_detect(CRT_1f, "Emily")==TRUE) {
    
    Data_allocation <- "1"
    
  } else{
    if (str_detect(CRT_1f, "EMILY")==TRUE) {
      
      Data_allocation <- "1"
      
    } else{
      if (str_detect(CRT_1f, "emily")==TRUE) {
        
        Data_allocation <- "1"
        
      } else{
        Data_allocation <- "0"
      }
    }
  }
}




# Create dummy variables for storing CRT scores
data_crt_1$CRT_1a <- NA
data_crt_1$CRT_1b <- NA
data_crt_1$CRT_1c <- NA
data_crt_1$CRT_1d <- NA
data_crt_1$CRT_1e <- NA
data_crt_1$CRT_1f <- NA

# Run the loops to calculate scores for each item
for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1a[i] <- CRT_1a_Function(CRT_1a = data_crt_1$C1a[i])
}

for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1b[i] <- CRT_1b_Function(CRT_1b = data_crt_1$C1b[i])
}

for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1c[i] <- CRT_1c_Function(CRT_1c = data_crt_1$C1c[i])
}

for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1d[i] <- CRT_1d_Function(CRT_1d = data_crt_1$C1d[i])
}

for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1e[i] <- CRT_1e_Function(CRT_1e = data_crt_1$C1e[i])
}

for (i in 1:nrow(data_crt_1)) {
  data_crt_1$CRT_1f[i] <- CRT_1f_Function(CRT_1f = data_crt_1$C1f[i])
}

data_crt_1$CRT_1g[data_crt_1$C1g > 0] <- -999
data_crt_1$CRT_1g[data_crt_1$C1g == 0] <- 1
data_crt_1$CRT_1g[data_crt_1$CRT_1g == -999] <- 0


data_crt_1$CRT_1a <- as.numeric(data_crt_1$CRT_1a)
data_crt_1$CRT_1b <- as.numeric(data_crt_1$CRT_1b)
data_crt_1$CRT_1c <- as.numeric(data_crt_1$CRT_1c)
data_crt_1$CRT_1d <- as.numeric(data_crt_1$CRT_1d)
data_crt_1$CRT_1e <- as.numeric(data_crt_1$CRT_1e)
data_crt_1$CRT_1f <- as.numeric(data_crt_1$CRT_1f)
data_crt_1$CRT_1g <- as.numeric(data_crt_1$CRT_1g)

data_crt_1$CRT_Score <- data_crt_1$CRT_1a + data_crt_1$CRT_1b + data_crt_1$CRT_1c +
  data_crt_1$CRT_1d + data_crt_1$CRT_1e + data_crt_1$CRT_1f + 
  data_crt_1$CRT_1g

colnames(data_crt_1)

data_crt_2 <- subset(data_crt_1, select = c(uuid, CRT_Score))

mean(data_crt_2$CRT_Score)
# 4.13786

median(data_crt_2$CRT_Score)
# 4

s_m_262400278 <- merge(data25, data_crt_2, by = "uuid", all.x = TRUE)


head(s_m_262400278)

colnames(s_m_262400278)

# Creating  data set with responses to misinformation posts only
p_m_262400278 <- subset(s_m_262400278, Misinfo > 0)

# To save
# library(data.table)
# fwrite(p_m_262400278, file = "p_m_262400278_v1.csv", row.names=TRUE, quote=TRUE)
# fwrite(s_m_262400278, file = "s_m_262400278_v1.csv", row.names=TRUE, quote=TRUE)