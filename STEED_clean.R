###Auto-Steed clean

#################################################
################## load packages ################
#################################################
library(pdftools)
library(limma)
library(dplyr)
library(readxl)
library(rvest)
library(stringr)
library(htmltools)
library(htmlwidgets)
library(tidyverse)
library(writexl)
library(formattable)

#################################################
############### PDF import function #############
#################################################
read.pdf.EE <- function(src)
{
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  
  QTD_COLUMNS <- 2
  read_text <- function(text) {
    result <- ''
    #Get all index of " " from page.
    lstops <- gregexpr(pattern =" ",text)
    #Puts the index of the most frequents ' ' in a vector.
    stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
    #Slice based in the specified number of columns (this can be improved)
    for(i in seq(1, QTD_COLUMNS, by=1))
    {
      temp_result <- sapply(text, function(x){
        start <- 1
        stop <-stops[i] 
        if(i > 1)            
          start <- stops[i-1] + 1
        if(i == QTD_COLUMNS)#last column, read until end.
          stop <- nchar(x)+1
        substr(x, start=start, stop=stop)
      }, USE.NAMES=FALSE)
      temp_result <- trim(temp_result)
      result <- append(result, temp_result)
    }
    result
  }
  
  txt <- pdf_text(src)
  result <- ''
  for (i in 1:length(txt)) { 
    page <- txt[i]
    t1 <- unlist(strsplit(page, "\n"))      
    maxSize <- max(nchar(t1))
    
    t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
    result = append(result,read_text(t1))
  }
  result <- result[!result %in% ""]
  return(result)
}

read.pdf.EE(src)

#################################################
###### RoB mining tool preclinical studies ######
#################################################

### Regex ###
ALSmodel_regex <- c("G93A", "G86R", "G85R", "G37R", "SOD186R",
                    "Tg-SOD1", "SOD1 Tg", "SOD1/rag2", "\\brag2",
                    "Q331K", "\\bFUS\\b", "A315T", "\\bNLS8\\b", 
                    "wobbler", "\\bwr\\b", "\\bMeHg\\b", "\\bcycad")

MSmodel_regex <- c("(?i)cuprizone", "(?i)\\bEAE\\b", "(?i)experimental autoimmune encephalomyelitis",
                   "(?i)experimental allergic encephalomyelitis", "(?i)lysolecithin",
                   "(?i)delayed type hypersensitivity", "(?i)japanese macaque encephalomyelitis",
                   "(?i)JME", "(?i)lipopolysaccharide", "(?i)TMEV", "(?i)cytokine injection", "(?i)hyponatr(a*)emia", "(?i)NMO",
                   "(?i)optic neuritis", "(?i)ethidium bromide", "(?i)EtBr")


species_list <- c("\\b(?i)rat[^ihe]", "\\b(?i)marmoset(s*)", "\\b(?i)guinea(| )pig(s*)", "\\b(?i)macaque(s*)", "\\b(?i)rhesus(| )monkey(s*)",
                  "\\b(?i)swine(s*)", "\\b(?i)dog(s*)", "\\b(?i)cat(s|\\b)[^.]", "\\b(?i)rabbit(s*)", "\\b(?i)primate(s*)",
                  "\\b(?i)hamster(s*)", "\\b(?i)sheep", "\\b(?i)mouse", "\\b(?i)mice", "\\b(?i)mini( *)pig")

female_regex <- "\\b(?i)female(s|)\\b"
only_female_regex <- "(?i)only.{1,10}\\bfemale(s|)\\b"
male_regex <- "\\b(?i)male(s|)\\b"
only_male_regex <- "(?i)only.{1,10}\\bmale(s|)\\b"
sex_regex <- c("(?i)either sex", "(?i)both sexes", "(?i)both genders", "(?i)either gender")


histology_list <- c(
  "(?i)histo(log(y|logical)|patholog(y|ic))",
  "(?i)staining",
  "(?i)(electron|confocal) microscopy",
  "(?i)immuno(histo|e|fluorescence|staining(s?))chemistry",
  "(?i)nissl",
  "(?i)hematoxylin(-eosin|)?",
  "(?i)eosin",
  "(?i)h&e"
)

behaviour_list <- c(
  "(?i)(digigait|treadmill|kinematics|grip (strength|test|meter)|rota(-|)rod|motor (performance|coordination|dysfunction)|hang (wire|ing (endurance|wire))|pole (test|climbing)|gait analysis|catwalk|grid hanging|climbing pole|neurological score|runtime|open field)",#Movement and performance
  "(?i)(swim test|morris (water )?maze|shirpa|reflex test|walking speed|stair( |)case|clinical (score(s?)|assessment|signs|symptoms|grading|scoring|evaluation)|neurological (status|deficit)|visual acuity|ambulation)",#Clinical assessment
  "(?i)(paralysis|flaccid|weakness|posture|moribund|ataxia|paraplegia|paresis|disability|examined clinically|disability score)"#physical condition and disability
)

imaging_list <- c(
  "(?i)(MR imaging|magnetic resonance imaging|\\bMRI\\b)",
  "(?i)(diffusion (tensor|kurtosis) imaging|diffusion( |-)weighted imaging|\\bDTI\\b|\\bDWI\\b)",
  "(?i)\\b(NODDI|MTR)\\b"
)

regex_removal_rob <- paste(
  "(powerful|",
  "power (level|level(s?)|wave(s?))|",
  "random (effect(s?)|primer(s?)|hexamer(s?)|variable(s?)|coefficient(s?)|",
  "variation|network)|",
  "(randomized(-controlled| controlled| control))|",
  "(blindness|double(-| )blind))",
  sep = ""
)

#Rob function
src = pdf.rob.vector[pdf.i]
pdf.extraction <- function(src)
{
  temp.x <- read.pdf.EE(src = src)
  
  #this creates a vector with 1 line as a element
  
  ############################################
  ########## Define paper sections ###########
  ############################################
  
  #Abstract
  
  if(length(grep("abstract|a b s t r a c t", temp.x, ignore.case = T)) == 0)
  {
    Abstract.start <- 1
  }else{
    Abstract.start <- min(grep("abstract|a b s t r a c t", temp.x, ignore.case = T))
  }
  
  #Introduction
  
  if(length(grep("Introduction", temp.x, ignore.case = T)) == 0)
  {
    Introduction.start <- 1
  }else{
    Introduction.start <- min(grep("Introduction", temp.x, ignore.case = T))
  }
  
  #Methods
  
  if(length(grep("m(\\s*)a(\\s*)t(\\s*)e(\\s*)r(\\s*)i(\\s*)a(\\s*)l(\\s*)s(\\s*)a(\\s*)n(\\s*)d(\\s*)m(\\s*)e(\\s*)t(\\s*)h(\\s*)o(\\s*)d(\\s*)s", temp.x, ignore.case = T)) == 0)
  {
    if(length(grep("methods", temp.x, ignore.case = T)[grep("methods", temp.x, ignore.case = T) > 33]) == 0)
    {
      Method.start <- 33
    }else{
      Method.start <- min(grep("methods", temp.x, ignore.case = T)[grep("methods", temp.x, ignore.case = T) > 33])
    }
  }else{
    Method.start <- min(grep("m(\\s*)a(\\s*)t(\\s*)e(\\s*)r(\\s*)i(\\s*)a(\\s*)l(\\s*)s(\\s*)a(\\s*)n(\\s*)d(\\s*)m(\\s*)e(\\s*)t(\\s*)h(\\s*)o(\\s*)d(\\s*)s", temp.x, ignore.case = T))
  }
  
  #Results --> most difficult to define results start since "result" appears commonly in scientific texts
  
  if(length(grep("Results", temp.x, ignore.case = T)[grep("Results", temp.x, ignore.case = T) > 33]) == 0)
  {
    Results.start <- 33
  }
  if(length(grep("Results", temp.x, ignore.case = T)[grep("Results", temp.x, ignore.case = T) > 33]) > 0)
  {
    Results.start <- min(grep("Results", temp.x, ignore.case = T)[grep("Results", temp.x, ignore.case = T) > 33])
  }
  
  #Discussion
  
  if(length(grep("Discussion", temp.x, ignore.case = T)[grep("Discussion", temp.x, ignore.case = T) > 33]) == 0)
  {
    if(length(grep("Discussion", temp.x, ignore.case = T)[grep("Discussion", temp.x, ignore.case = T) > 33]) == 0)
    {
      Discussion.start <- 33
    }else{
      Discussion.start <-  min(grep("Results", temp.x, ignore.case = T)[grep("Discussion", temp.x, ignore.case = T) > 33])
    }
  }else{
    Discussion.start <- min(grep("Discussion", temp.x, ignore.case = T)) 
  }
  
  #References
  
  if(length(grep("R(\\s*)e(\\s*)f(\\s*)e(\\s*)r(\\s*)e(\\s*)n(\\s*)c(\\s*)e(\\s*)s", temp.x, ignore.case = T)[grep("R(\\s*)e(\\s*)f(\\s*)e(\\s*)r(\\s*)e(\\s*)n(\\s*)c(\\s*)e(\\s*)s", temp.x, ignore.case = T) > 33]) == 0)
  {
    References.start <- length(temp.x)
  }else{
    References.start <- min(grep("R(\\s*)e(\\s*)f(\\s*)e(\\s*)r(\\s*)e(\\s*)n(\\s*)c(\\s*)e(\\s*)s", temp.x, ignore.case = T)[grep("R(\\s*)e(\\s*)f(\\s*)e(\\s*)r(\\s*)e(\\s*)n(\\s*)c(\\s*)e(\\s*)s", temp.x, ignore.case = T) > 33])
  }
  
  ############################################
  ############# Paper mapping ################
  ############################################ 
  
  #Abstract
  
  if(Abstract.start == 1)
  {
    if(length(grep("Introduction", temp.x, ignore.case = T)) == 0){temp.x.abstract <- temp.x[1:80]}
    else{temp.x.abstract <- temp.x[1:Introduction.start]}}
  if(Abstract.start > 1)
  {
    if(length(grep("Introduction", temp.x, ignore.case = T)) == 0){temp.x.abstract <- temp.x[Abstract.start:80]}
    else{temp.x.abstract <- temp.x[Abstract.start:Introduction.start]}}
  
  temp.x.abstract <- paste(unlist(temp.x.abstract), sep = " ")
  temp.x.abstract <- paste(temp.x.abstract, collapse = " ")
  
  #Introduction
  
  if(Introduction.start < Method.start){intro.range <- Introduction.start:Method.start
  }else{intro.range <- Introduction.start+80}#which value covers 90% of all Intros? 80 is a raw estimate
  
  temp.x.intro <- temp.x[intro.range]
  temp.x.intro <- paste(unlist(temp.x.intro), sep = " ")
  temp.x.intro <- paste(temp.x.intro, collapse = " ")
  
  #Experimental (in most cases, you want to search methods and results for our terms, experimental = methods + results)
  
  if(Discussion.start < Method.start)
  {
    experimental.range <- Method.start:References.start
  }else{
    experimental.range <- Method.start:Discussion.start
  }
  
  temp.x.method <- temp.x[experimental.range]
  temp.x.method <- paste(unlist(temp.x.method), sep = " ")
  temp.x.method <- paste(temp.x.method, collapse = " ")
  
  #Whole paper w/o References
  
  paper.range <- Abstract.start:References.start
  paper.range.full <- 1:length(temp.x)
  temp.x.paper <- temp.x[paper.range]
  temp.x.paper <- paste(unlist(temp.x.paper), sep = " ")
  temp.x.paper <- paste(temp.x.paper, collapse = " ")
  temp.x.paper.full <- temp.x[paper.range.full]
  temp.x.paper.full <- paste(unlist(temp.x.paper.full), sep = " ")
  temp.x.paper.full <- paste(temp.x.paper.full, collapse = " ")
  
  temp.x.method <- str_remove_all(temp.x.method, regex(regex_removal_rob, ignore_case = T))
  temp.x.paper <- str_remove_all(temp.x.paper, regex(regex_removal_rob, ignore_case = T))
  temp.x.paper.full <- str_remove_all(temp.x.paper.full, regex(regex_removal_rob, ignore_case = T))
  
  ############################################
  ################ Metadata ##################
  ############################################
  
  #file title
  file.title.temp <- str_extract(pdf.rob.vector[pdf.i], regex("/.+", ignore_case = T))
  file.title <- str_extract(file.title.temp, regex("[^/].+"))
  
  #author
  if(str_detect(pdf.rob.vector[pdf.i], regex("/[a-z| ]+-", ignore_case = T)) == F){
    author <- NA
  }else{
    author.temp <- str_extract(pdf.rob.vector[pdf.i], regex("/[a-z| ]+-", ignore_case = T))
    author <- str_extract(author.temp, regex("[a-z]+", ignore_case = T))
  }
  
  #year
  if(str_detect(pdf.rob.vector[pdf.i], regex("[^\\d| ]\\d\\d\\d\\d[^\\d| ]", ignore_case = T)) == T){
    year.temp <- str_extract(pdf.rob.vector[pdf.i], regex("[^\\d| ]\\d\\d\\d\\d[^\\d| ]", ignore_case = T))
    year.temp <- str_extract(year.temp, regex("\\d\\d\\d\\d", ignore_case = T))
    year <- ifelse(year.temp < 2019 & year.temp > 1900, year.temp, NA)
  }else{
    year.temp <- t(strsplit2(temp.x.abstract, " "))
    year.temp <- gsub(";", "", year.temp)
    year.temp <- gsub(",", "", year.temp)
    
    year.temp <- as.numeric(year.temp)
    year.temp <- year.temp[!is.na(year.temp)]
    year.temp <- year.temp[nchar(year.temp) == 4]
    year.temp <- year.temp[ year.temp < 2019]
    year.temp <- year.temp[ year.temp > 1900]
    year.temp <- max(year.temp, na.rm = T)
    year <- ifelse(year.temp < 2019 & year.temp > 1900, year.temp, NA)
  }
  
  #paper title
  if(str_detect(pdf.rob.vector[pdf.i], regex("-[a-z| ]+\\.", ignore_case = T)) == F){
    paper.title <- NA
  }else{
    paper.title.temp <- str_extract(pdf.rob.vector[pdf.i], regex("-[a-z| ]+\\.", ignore_case = T))
    paper.title <- str_extract(paper.title.temp, regex("[a-z| ]+", ignore_case = T))
  }
  
  #doi
  if(str_detect(temp.x.method, regex("10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+", ignore_case = T)) == T){
    doi <- str_extract_all(temp.x.method, regex("10.\\d{4,9}/[-._;()/:a-z0-9A-Z]+", ignore_case = T))
  }else{
    doi <- NA
  }
  
  #E-mail
  if(str_detect(toString(temp.x.paper), regex("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", ignore_case = T)) == F){
    email <- NA
  }else{
    email.temp <- str_extract_all(temp.x.paper, regex("([_a-z0-9-]+(\\.[_a-z0-9-]+)*@[a-z0-9-]+(\\.[a-z0-9-]+)*(\\.[a-z]{2,4}))", ignore_case = T))
    email.temp <- Filter(length, email.temp)
    email <- paste(email.temp, collapse = "; ")}
  
  ############################################
  ########### Experimental data ##############
  ############################################
  
  #Experimental model
  model.count.temp <- vector()
  model.temp <- vector()
  for(numCount in seq_along(MSmodel_regex)) 
  {
    model.count.temp[[numCount]] <- sum(str_count(temp.x.method, regex(MSmodel_regex[[numCount]], ignore_case = F)))#ignore case!
    model.temp[[numCount]] <- str_extract(paste(temp.x.method, collapse = " "), regex(MSmodel_regex[[numCount]], ignore_case = F))#ignore case!
  }
  model.temp.df <- as.data.frame(t(rbind(model.temp, model.count.temp)), stringsAsFactors = F)
  model.temp.df$model.count.temp <- as.numeric(model.count.temp)
  model.temp.df$model.temp <- as.character(model.temp)
  model.temp.df <- model.temp.df[order(desc(model.temp.df$model.count.temp)),]
  
  model1 <- paste(model.temp.df$model.temp[1])
  model_count1 <- paste(model.temp.df$model.count.temp[1])
  model2 <- paste(model.temp.df$model.temp[2])
  model_count2 <- paste(model.temp.df$model.count.temp[2])
  
  if(model1 == "NA"){model1 <- "not reported"}
  if(model2 == "NA"){model2 <- "not reported"}
  
  #Species
  species.count.temp <- vector()
  species.temp <- vector()
  for(numCount in seq_along(species_list)) 
  {
    species.count.temp[[numCount]] <- sum(str_count(temp.x.method, regex(species_list[[numCount]])))
    species.temp[[numCount]] <- str_extract(temp.x.method, regex(species_list[[numCount]]))
  }
  species.temp.df <- as.data.frame(t(rbind(species.temp, species.count.temp)), stringsAsFactors = F)
  species.temp.df$species.count.temp <- as.numeric(species.count.temp)
  species.temp.df$species.temp <- as.character(species.temp)
  species.temp.df <- species.temp.df[order(desc(species.temp.df$species.count.temp)),]
  species.temp.df <- species.temp.df[!(species.temp.df$species.count.temp == 0),]
  
  species1 <- paste(species.temp.df$species.temp[1])
  species_count1 <- paste(species.temp.df$species.count.temp[1])
  species2 <- paste(species.temp.df$species.temp[2])
  species_count2 <- paste(species.temp.df$species.count.temp[2])
  
  if(species1 == "NA"){species1 <- "not reported"}
  if(species2 == "NA"){species2 <- "not reported"}
  
  #Sex
  has_female <- str_subset(temp.x.method, regex(female_regex, ignore_case = T))
  has_female <- str_extract(temp.x.method, regex(female_regex, ignore_case = T))
  has_female <- paste(unlist(has_female), collapse = ";")
  ifelse(str_detect(has_female, regex(female_regex, ignore_case = T)) == T,
         female <- "female", female <- 0)
  
  has_male <- str_subset(temp.x.method, regex(male_regex, ignore_case = T))
  has_male <- str_extract(temp.x.method, regex(male_regex, ignore_case = T))
  has_male <- paste(unlist(has_male), collapse = ";")
  ifelse(str_detect(has_male, regex(male_regex, ignore_case = T)) == T,
         male <- "male", male <- 0)
  
  if(sum(str_detect(female, regex(female_regex, ignore_case = T))) > 0 &
     sum(str_detect(male, regex(male_regex, ignore_case = T))) == 0){
    sex.test <- "female"
  }
  if(sum(str_detect(female, regex(female_regex, ignore_case = T))) == 0 &
     sum(str_detect(male, regex(male_regex, ignore_case = T))) > 0){
    sex.test <- "male"
  }
  if(sum(str_detect(female, regex(female_regex, ignore_case = T))) > 0 &
     sum(str_detect(male, regex(male_regex, ignore_case = T))) > 0){
    sex.test <- "female and male"
  }
  if(sum(str_detect(female, regex(female_regex, ignore_case = T))) == 0 &
     sum(str_detect(male, regex(male_regex, ignore_case = T))) == 0){
    sex.test <- "not reported"
  }
  if(sum(str_detect(temp.x.method, regex(sex_regex, ignore_case = T))) > 0){
    sex.test <- "female and male"
  }
  if(sum(str_detect(temp.x.method, regex(only_female_regex, ignore_case = T))) > 0){
    sex.test <- "female"
  }
  if(sum(str_detect(temp.x.method, regex(only_male_regex, ignore_case = T))) > 0){
    sex.test <- "male"
  }
  
  #Outcome
  has_histology <- str_extract(temp.x.method, regex(histology_list, ignore_case = T))
  has_histology <- paste(unlist(has_histology), collapse = ";")
  ifelse(str_detect(has_histology, regex(histology_list, ignore_case = T)) == F, histology_outcome <- "not reported", histology_outcome <- "histology")
  
  has_behaviour <- str_extract(temp.x.method, regex(behaviour_list, ignore_case = T))
  has_behaviour <- paste(unlist(has_behaviour), collapse = ";")
  ifelse(str_detect(has_behaviour, regex(behaviour_list, ignore_case = T)) == F, behaviour_outcome <- "not reported", behaviour_outcome <- "behaviour")
  
  has_imaging <- str_extract(temp.x.method, regex(imaging_list, ignore_case = T))
  has_imaging <- paste(unlist(has_imaging), collapse = ";")
  ifelse(str_detect(has_imaging, regex(imaging_list, ignore_case = T)) == F, imaging_outcome <- "not reported", imaging_outcome <- "imaging")
  

  ############################################
  ############## Risk of Bias ################
  ############################################
  
  #Welfare
  if(str_detect(temp.x.method, regex("approved|approval|carried out in accordance|conducted in accordance|in accordance|animal experiments were performed|conformed to guidelines|complied with relevant|in compliance with the|animal handling conformed|met the state regulations|in agreement with|in strict adherence|in adherence", ignore_case = T)) == T){
    welfare <- "yes"
  }else{
    welfare <- "not reported"
  }
  
  if(sum(str_count(temp.x.method, regex("approved|approval|carried out in accordance|conducted in accordance|performed in strict concordance|in accordance|according to guidelines|conformed to the guidelines", ignore_case = T))) > 1){
    welfare_signal <- "Manual check recommended"
  }else{
    welfare_signal <- "No manual check required"
  }
  
  #Blinding
  if(str_detect(temp.x.method, regex("blinded|were blind|blind", ignore_case = T)) == T){
    blinding <- "yes"
  }else{
    blinding <- "not reported"
  }

  if(sum(str_count(temp.x.method, regex("blinded|were blind|blind", ignore_case = T))) > 1){
    blinding_signal <- "Manual check recommended"
  }else{
    blinding_signal <- "No manual check required"
  }
  
  #Randomization
  if(str_detect(temp.x.method, regex("random", ignore_case = T)) == T){
    randomization <- "yes"
  }else{
    randomization <- "not reported"
  }
  
  if(sum(str_count(temp.x.method, regex("random", ignore_case = T))) > 1){
    randomization_signal <- "Manual check recommended"
  }else{
    randomization_signal <- "No manual check required"
  }
  
  #sample size calculation
  if(str_detect(temp.x.paper.full, regex("achieve.{1,10}power|sample size calculation|power analysis|study power|sample size|power calculation", ignore_case = T)) == T){
    powertest <- "yes"
  }else{
    powertest <- "not reported"
  }
  
  if(sum(str_count(temp.x.paper.full, regex("achieve.{1,10}power|sample size calculation|power analysis|study power|sample size", ignore_case = T))) > 1){
    powertest_signal <- "Manual check recommended"
  }else{
    powertest_signal <- "No manual check required"
  }
  
  #ARRIVE guidelines
  if(str_detect(temp.x.paper, regex("ARRIVE", ignore_case = F)) == T){#ignore case!
    ARRIVE <- "yes"
  }else{
    ARRIVE <- "not reported"
  }
  ##CAVE: AFTER 2010
  if(sum(str_count(temp.x.paper, regex("ARRIVE", ignore_case = F))) > 1){
    ARRIVE_signal <- "Manual check recommended"
  }else{
    ARRIVE_signal <- "No manual check required"
  }
  
  #conflict of interest
  if(str_detect(temp.x.paper.full, regex("conflict[s| ] ?of interest|disclosures|disclosure|competing interests|nothing to disclose|received grant[s| ]|disclosure statement|financial disclosure|financial interests", ignore_case = T)) == T){
    disclosure <- "yes"
  }else{
    disclosure <- "not reported"
  }
  
  if(sum(str_count(temp.x.paper.full, regex("conflict[s| ] ?of interest|disclosures|competing interests|nothing to disclose|received grant[s| ]|disclosure statement", ignore_case = T))) > 1){
    disclosure_signal <- "Manual check recommended"
  }else{
    disclosure_signal <- "No manual check required"
  }
  
  #Data availability
  if(str_detect(temp.x.paper.full, regex("data availability|availability of data", ignore_case = T)) == T){
    availability <- "yes"
  }else{
    availability <- "not reported"
  }
  
  ############################################
  ################# OUTPUT ###################
  ############################################
  
  return.x <- c(file.title, author, year, paper.title, 
                model1, model_count1, model2, model_count2,
                species1, species_count1, species2, species_count2,
                sex.test, histology_outcome, behaviour_outcome, imaging_outcome,
                randomization, randomization_signal,
                blinding, blinding_signal,
                welfare, welfare_signal,
                disclosure, disclosure_signal,
                powertest, powertest_signal,
                ARRIVE, ARRIVE_signal,
                availability,
                doi[[1]][1], email,
                Abstract.start, Introduction.start, Method.start, Results.start, Discussion.start,
                paste(c(min(experimental.range), max(experimental.range)), collapse=" - "),
                paste(c(min(paper.range.full), max(paper.range.full)), collapse=" - "))
  names(return.x) <- c("File title", "First author", "Year","Paper title", 
                       "Model 1", "Model count 1", "Model 2", "Model count 2",
                       "Species 1", "Species count 1", "Species 2", "Species count 2",
                       "Sex", "Outcome histology", "Outcome behaviour", "Outcome imaging",
                       "Randomization", "Randomization_QC",
                       "Blinding", "Blinding_QC",
                       "Welfare", "Welfare_QC",
                       "Conflict", "Conflict_QC",
                       "Samplesize", "Sample_QC",
                       "ARRIVE", "ARRIVE_QC",
                       "Data_availability_statement",
                       "doi", "email", 
                       "Abstract start", "Introduction start", "Methods start", "Results start", "Discussion start",
                       "Methods range", "Paper range")
  return(return.x)
}


#################################################
####### Apply function to reference sets ########
#################################################
#test set 1: define folder with PDFs
pdf.rob.vector <- list.files("C:/Users/benja/Dropbox/Research/Experiments/Karolinska/ALS_SystematicReview/Animal_study/Validation_sample/", full.names = T)
pdf.rob.vector <- pdf.rob.vector[grep("pdf", pdf.rob.vector, ignore.case = T)]
pdf.rob.vector <- pdf.rob.vector[!grepl("pdftools", pdf.rob.vector)]
pdf.rob.vector
pdf.extraction(src = pdf.rob.vector[1])

#Extract: apply function to PDF test set
mined.rob <- vector()

for(pdf.i in 1:length(pdf.rob.vector)) 
{
  print(pdf.i)
  print(pdf.rob.vector[pdf.i])
  mined.rob <-  rbind(mined.rob, pdf.extraction(src = pdf.rob.vector[pdf.i]))
}

#Extracted data (as dataframe)
mined.rob <- as.data.frame(mined.rob)


