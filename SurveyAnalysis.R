library(readxl)
library(dplyr)
library(tidyr)
library(tm)
library(lubridate)
library(plyr)
library(data.table)


# Work computer
path <- "C:/PTO/Survey Data"

# Home computer
path <- c("C:/MyStuff/DataScience/Projects/PTOSurvey/Data")
setwd(path)


# Read in file and remove CollectorID
 
qualdata <- read_excel("STICSurveyData20150304.xlsx", sheet = 1, col_names = TRUE, na = " ")


# Convert columns to appropriate type
# Make all open response except worker classification columns characters
data1[ ,c(15, 35, 76:77, 107:108, 119, 146, 163, 173, 179)] <- as.character(data1[ , c(15, 35, 76:77, 107:108, 119, 146, 163, 173, 179)])                              

# Make Start Date & End Date Date class

data1$StartDate <- mdy_hms(data1$StartDate)
data1$EndDate <- mdy_hms(data1$EndDate)

# Make a sequence for converting remaining columns to factors

sequ <- seq(1:179)
sequ <- sequ[-c(3:4 ,15, 35, 76:77, 107:108, 119, 146, 163, 173, 179)]

# Make everything else Factors

data1[ , sequ] <- lapply(data1[ , sequ], factor)

# Read names from Excel file and name columns

# Read in another version of spreadsheet for quantifiable calculations

quantdata <- read_excel("STIC Survey Data (final) 20150304.xlsx", sheet = 1, col_names = TRUE, na = " ")


col_names <- read_excel("STIC Survey Data (final) 20150304.xlsx", sheet = 3, col_names = FALSE)
col_names <- col_names$X0
names(data1) <- col_names

# Create a df with the basic info for each sub.df

base_info <- data.frame(Tech.Center = data1[ , 5],
                        Other.Tech.center = data1[ , 6],
                        Work.Group = data1[ , 7],
                        Grade = data1[ , 8],
                        Tenure = data1[ , 9],
                        Prim.Location = data1[ , 10],
                        Other.Location = data1[ , 11],
                        Current.Position = data1[ , 12],
                        Other.Position = data1[ , 13]
                        )


datalist <- list(Use.of.STIC.serv <- data1[ , c(5:13, 14:15)],
                  Use.of.Following.Sources <- data1[, c(5:13, 16:20)],
                  Resources.Beyond.Norm.Search.Freq <- data1[, c(5:13, 21:27)],
                  Interaction.Importance.Type <- data1[, c(5:13, 28:35)],
                  Mobile.Dev.Use <- data1[ , c(5:13, 36:37)],
                  Searching.Freq.Satisf <- data1[, c(5:13, 38:55)],
                  Doc.Retr.Trans.Freq.Satisf <- data1[, c(5:13, 56:65)],
                  Training.Freq.Satisf <- data1[, c(5:13, 66:77)],
                  STIC.Tools.Freq.Satisf <- data1[ , c(5:13, 78:109)],
                  STIC.Searcher <- data1[ , c(5:13, 110:111)],
                  Resulsts.Receive.Pref <- data1[ , c(5:13, 112:118)],
                  Suggestions.For.STIC.Searches <- data1[ , c(5:13, 119)],
                  SSE.Freq.Satisf  <- data1[ , c(5:13, 120:121)],
                  SSE.Service.Used.Past.12.Mon <- data1[ , c(5:13, 122:126)],
                  STIC.Usage.Ever.Reason.For.Stopping <- data1[ , c(5:13, 127:128)],
                  Examiners.Supervised.Suggested.STIC.Services <- data1[ , c(5:13, 129:138)],
                  Reasons.Not.To.Recommend <- data1[ , c(5:13, 139:146)],
                  Training.Awareness.And.Interest <- data1[ , c(5:13, 147:148)],
                  Training.Interests <- data1[ , c(5:13, 149:163)],
                  Training.Satisf <- data1[ , c(5:13, 164:173)],
                  STIC.Overall.Customer.Service <- data1[ , c(5:13, 174:178)],
                  Final.Comments.And.Suggestions <- data1[ , c(5:13, 179)]
                )




# A function to convert all responses in Never, Rarely, Occasionally,
# Frequently, and Always format to numerical scale and find average

nrofa <- function(x){
        x1 <- gsub("Never", 1, x)1
        x2 <- gsub("Rarely", 2, x1)
        x3 <- gsub("Occasionally", 3, x2)
        x4 <- gsub("Frequently", 4, x3)
        x5 <- gsub("Always", 5, x4)
        x6 <- as.numeric(x5)
        mean(x6, na.rm = TRUE)
}


imp_avg <- function(x){
  x1 <- gsub("Not important at all", 1, x)
  x2 <- gsub("Somewhat important", 2, x1)
  x3 <- gsub("No Opinion: I am indifferent", 3, x2)
  x4 <- gsub("Moderately important", 4, x3)
  x5 <- gsub("Critical", 5, x4)
  x6 <- as.numeric(x5)
  mean(x6, na.rm = TRUE)
}

# This needs to be fixed

STIC_use_avg <- function(x){
  x1 <- gsub("No", 1, x)
  x2 <- gsub("I have used STIC services in the past, but don’t any longer", 2, x1)
  x3 <- gsub("Yes, I use STIC services occasionally", 3, x2)
  x4 <- gsub("Yes, I use STIC services regularly", 4, x3)
  x5 <<- as.numeric(x4)
  mean(x5, na.rm = TRUE)
}

# Frequency of Use average 


freq_avg <- function(x){
  x1 <- gsub("Never", 1, x)
  x2 <- gsub("Just once", 2, x1)
  x3 <- gsub("A couple of times during the year", 3, x2)
  x4 <- gsub("Once a quarter", 4, x3)
  x5 <- gsub("Once a month", 5, x4)
  x6 <- gsub("Weekly", 6, x5)
  x7 <- as.numeric(x6)
  mean(x7, na.rm = TRUE)
}


satis_avg <- function(x){
  x1 <- gsub("Very Dissatisfied", 1, x)
  x2 <- gsub("Dissatisfied", 2, x1)
  x3 <- gsub("Neither Satisfied nor Dissatisfied", 3, x2)
  x4 <- gsub("Satisfied", 4, x3)
  x5 <- gsub("Very Satisfied", 5, x4)
  x6 <<- as.numeric(x5)
  mean(x6, na.rm = TRUE)
}

# Text mining portion. Used for comment/open ended responses. 
# Word frequencies

# Remove the no responses and paste all responses into one character vector

prep <- function(x){
    x1 <- na.omit(x)
    x2 <<- paste(x1, sep = "", collapse = "")
}

# Corpus cleaning function

clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "stic"))
  return(corpus)
}

# Create the corpus and clean it for TDM

corp_create <- function(x){
  txtsource <- VectorSource(x)
  txtcorpus <- VCorpus(txtsource)
  clean_corp <<- clean_corpus(txtcorpus)
}

# Create TDM, matrix, calculate frequencies and create plot


Count_Graphic <- function(x){
gao_tdm <- TermDocumentMatrix(clean_corp)
gao_m <- as.matrix(gao_tdm)
term_frequency <- rowSums(gao_m)
term_frequency <- sort(term_frequency, decreasing = TRUE)
barplot(term_frequency[1:10])
term_frequency[1:10]
}


# Gather the necessary columns

comments <- quantdata[ , c(15, 77, 109, 119, 128,179)]

# Frequency for the question: "If you have never used STIC services
# or if you have stopped using STIC services, please indicate why:"

prep_Q1 <- prep(comments[, 1])
corp_Q1 <- corp_create(prep_Q1)


