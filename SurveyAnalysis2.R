library(data.table)
library(ggplot2)
library(readxl)



# At home
path <- c("C:/MyStuff/DataScience/Projects/PTOSurvey/Data")
setwd(path)

quantdata <- read_excel("STIC_Survey Data_2016 07 07_v4 (002).xlsx", sheet = 5, col_names = TRUE)
quantdata <- as.data.table(quantdata)
quantdata[, c("Currentjobposition", "TechnologyCenter") := qualdata$`Current job position`, qualdata$`Technology Center:`]



# Overall satisfaction by position
overall.satis <- qualdata[, .N, by = c("Responsiveness", "Currentjobposition")]
JE <- overall.satis[Currentjobposition == "Junior Examiner"]
SPE <- overall.satis[Currentjobposition == "SPE"]
PE <-  overall.satis[Currentjobposition == "Partial Sig or Primary Examiner"]
QAS <- overall.satis[Currentjobposition == "QAS"]
Tr <- overall.satis[Currentjobposition == "Trainer"]
O <- overall.satis[Currentjobposition == "Other"]


# Common reasons not to recommend a STIC service 

qualdata[, .N, by = ExaminersdonotneedNPL]
qualdata[, .N, by = NotawareofservicesSTICprovides]
qualdata[, .N, by = DontknowhowtoaccessSTICservices]
qualdata[, .N, by = ReasonsyoumightnotrecommendaSTICserviceExaminersdonotneedforeignpatents]


# Tool usage vary by grade 



# Create data.table with tool/resource frequencies and current grade
tooldata <- quantdata[, .(`STIC Resources: Equipment in the EIC, such as a fax machine, photocopier, scanner, etc. - Frequency of Use`,
              `STIC Resources: E2D2 (Examiner's Electronic Digest Database) - Frequency of Use`,
              `STIC Resources: ProQuest Foreign Patent Finder to obtain a foreign patent - Frequency of Use`,
              `STIC Resources: TheTranslations web site to obtain a machine translation - Frequency of Use`,
              `STIC Resources: A commercial database:  Lexis-Nexis (for business methods searching) - Frequency of Use`,
              `STIC Resources: A commercial database:  STN or SciFinder (for chemical searching) - Frequency of Use`,
              `STIC Resources: A commercial database:  ProQuest Dialog - Frequency of Use`,
              `STIC Resources: A technical or industry standard or protocol - Frequency of Use`,
              `STIC Resources: Google Scholar (a portal to search and retreive STIC subscriptions) - Frequency of Use`,
              `STIC Resources: An e-journal or database from STIC's NPL collection (such as IEEE Xplore, ScienceDirect, EbscoHost, ProQuest or IP.com) - Frequency of Use`,
              `STIC Resources: An e-book from STIC's NPL collection (such as Safari Books Online or Knovel) - Frequency of Use`,
              `STIC Resources: A print book from STIC's collection - Frequency of Use`,
              `STIC Resources: The STIC online catalog to search for a book or journal - Frequency of Use`,
              `STIC Resources: The STIC NPL web site - Frequency of Use`,
               `Current Grade` = qualdata$CurrentGrade)]

#Take mean of each tool by Current Grade

# Equipment
equip.by.grade <- tooldata[, .(Usage = mean(`STIC Resources: Equipment in the EIC, such as a fax machine, photocopier, scanner, etc. - Frequency of Use`, 
                na.rm = TRUE)), 
                by = `Current Grade`][order(Usage, decreasing = TRUE)]
equip.by.grade <- order(equip.by.grade$Usage, decreasing = TRUE)
equip.by.grade <- equip.by.grade[`Current Grade` != "SES"]


ggplot(equip.by.grade, aes(x = reorder(`Current Grade`, -Usage), y = Usage)) + 
          geom_bar(stat = "identity", fill = "#9933FF") +
          ggtitle("Equipment Usage by Grade") +
          labs(x = "Grade", y = "Usage")


# E2D2
tooldata[, mean(`STIC Resources: E2D2 (Examiner's Electronic Digest Database) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]
# Proquest Foreign patent
tooldata[, mean(`STIC Resources: ProQuest Foreign Patent Finder to obtain a foreign patent - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Translations
tooldata[, mean(`STIC Resources: TheTranslations web site to obtain a machine translation - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Lexis-Nexis
tooldata[, mean(`STIC Resources: A commercial database:  Lexis-Nexis (for business methods searching) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# STN or SCIFinder
tooldata[, mean(`STIC Resources: A commercial database:  STN or SciFinder (for chemical searching) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Proquest Dialog
tooldata[, mean(`STIC Resources: A commercial database:  ProQuest Dialog - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Technical/Industry Protocol
tooldata[, mean(`STIC Resources: A technical or industry standard or protocol - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Online catalog for book or journal
tooldata[, mean(`STIC Resources: The STIC online catalog to search for a book or journal - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Google Scholar
tooldata[, mean(`STIC Resources: Google Scholar (a portal to search and retreive STIC subscriptions) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# E-journal/Databse form NPL
tooldata[, mean(`STIC Resources: An e-journal or database from STIC's NPL collection (such as IEEE Xplore, ScienceDirect, EbscoHost, ProQuest or IP.com) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# Print book
tooldata[, mean(`STIC Resources: A print book from STIC's collection - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# E-book from NPL
tooldata[, mean(`STIC Resources: An e-book from STIC's NPL collection (such as Safari Books Online or Knovel) - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]

# NPL website
tooldata[, mean(`STIC Resources: The STIC NPL web site - Frequency of Use`, 
                na.rm = TRUE), 
         by = `Current Grade`]


# Answering the question: What are the most commonly used tools by Tech Center?
setkey(tool.by.tc., `Tech Center`)

# Copy 'tooldata' from above and change the Current Grade to Technology Center
tool.by.tc <- quantdata[, .(`STIC Resources: Equipment in the EIC, such as a fax machine, photocopier, scanner, etc. - Frequency of Use`,
                          `STIC Resources: E2D2 (Examiner's Electronic Digest Database) - Frequency of Use`,
                          `STIC Resources: ProQuest Foreign Patent Finder to obtain a foreign patent - Frequency of Use`,
                          `STIC Resources: TheTranslations web site to obtain a machine translation - Frequency of Use`,
                          `STIC Resources: A commercial database:  Lexis-Nexis (for business methods searching) - Frequency of Use`,
                          `STIC Resources: A commercial database:  STN or SciFinder (for chemical searching) - Frequency of Use`,
                          `STIC Resources: A commercial database:  ProQuest Dialog - Frequency of Use`,
                          `STIC Resources: A technical or industry standard or protocol - Frequency of Use`,
                          `STIC Resources: Google Scholar (a portal to search and retreive STIC subscriptions) - Frequency of Use`,
                          `STIC Resources: An e-journal or database from STIC's NPL collection (such as IEEE Xplore, ScienceDirect, EbscoHost, ProQuest or IP.com) - Frequency of Use`,
                          `STIC Resources: An e-book from STIC's NPL collection (such as Safari Books Online or Knovel) - Frequency of Use`,
                          `STIC Resources: A print book from STIC's collection - Frequency of Use`,
                          `STIC Resources: The STIC online catalog to search for a book or journal - Frequency of Use`,
                          `STIC Resources: The STIC NPL web site - Frequency of Use`,
                          `Tech Center` = qualdata$`Technology Center:`)]

# Breakdown of mean of numerical scales for each tool by Tech Center
tool.score.tc <- tool.by.tc[, lapply(.SD, mean, na.rm = TRUE), by = "Tech Center"]
setkey(tool.score.tc, "Tech Center")

# Set columns desired
xlab.order <- c("1600", "1700", "2100", "2400", "2600", "2800", "2900", "3600", "3700", "3900 (CRU)", "Other")
tool.score.tc <- tool.score.tc[xlab.order]




# Graph function

BarGraphFunction <- function(data, x.axis, y.axis, color, xlabel, ylabel, title){
  ggplot(data, aes(x = x.axis, y = y.axis)) + 
    geom_bar(stat = "identity", fill = color) +
    ggtitle(title) +
    labs(x = xlabel, y = ylabel)
}


BarGraphFunction(tool.score.tc, x.axis = Tech Center, y.axis = `STIC Resources: The STIC NPL web site - Frequency of Use`,
                 color = "9933FF", xlab = "Tech Center", ylab = "Usage", title = "Usage by Tech Center of STIC's NPL Website")

                             
# Plot for NPL's website
ggplot(tool.score.tc, aes(x = `Tech Center`, y = `STIC Resources: The STIC NPL web site - Frequency of Use`)) + 
  geom_bar(stat = "identity", fill = "#9933FF") +
  ggtitle("Usage by Tech Center of STIC's NPL Website") +
  labs(x = "Tech Center", y = "Usage")
  
# Plot for Print book   
ggplot(tool.score.tc, aes(x = `Tech Center`, y = `STIC Resources: A print book from STIC's collection - Frequency of Use`)) + 
  geom_bar(stat = "identity", fill = "#9933FF") +
  ggtitle("Usage by Tech Center of STIC's NPL Website") +
  labs(x = "Tech Center", y = "Usage")
  