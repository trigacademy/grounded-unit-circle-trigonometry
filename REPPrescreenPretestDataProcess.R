library(ggplot2)
library(dplyr)
library(data.table)
library(jsonlite)
library(magrittr)
library(rvest)
library(tidyr)


allREPResults <- readRDS("allREPResults.rds")

##reduce to only pretest results
allData <- allREPResults[allREPResults$phase=="pre",]

#make sure created column is in date format for distinguishing between vocab and letter sets
allData$created <- substr(allData$created,start=1,stop=10)
allData$created <- as.Date(allData$created, "%Y-%m-%d")

#read in users and get userid list
userIDs <- read.csv("AllREPIDs.csv")
allStudents <- userIDs$TrigID

################################################figure rotation

figureRotation <- function(i) {
  
  if (i=="Rotate") {
    apiRotateTA <- allData[(allData$unit=="1") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  for(j in allStudents){
    
    pageData <- apiRotateTA[apiRotateTA$user_id==j, ]
    
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "single-stim", ]
    if("rt" %in% names(pageData) && "answer" %in% names(pageData) && "response" %in% names(pageData) && "correct" %in% names(pageData)){
      pageData$trial = pageData$trial_index+1
      pageData <- pageData[,c('trial',"rt", "answer", "response", "correct", "stimulus")]
      
      for (i in 1:2) {
        pageData[paste0('stim',i)] = sapply(pageData$stimulus,function(x){substr(html_attrs(html_nodes(read_html(x),'img'))[[i]]['src'],nchar('/static/img/z/')+1,nchar(x)-nchar('.jpg'))})
        pageData[paste0('shape',i)] = sapply(sapply(pageData[paste0('stim',i)],function(x){strsplit(x,'_')}),function(x){x[1]})
        pageData[paste0('axis',i)] = sapply(sapply(pageData[paste0('stim',i)],function(x){strsplit(x,'_')}),function(x){x[2]})
        pageData[paste0('rotate',i)] = sapply(sapply(pageData[paste0('stim',i)],function(x){strsplit(x,'_')}),function(x){x[3]})
        pageData[paste0('mirror',i)] = sapply(sapply(pageData[paste0('stim',i)],function(x){strsplit(x,'_')}),function(x){x[4]})
      }
      
      pageData <- cbind(pageData, data.frame(user=rep(j,nrow(pageData))))
      data <- rbind(data,pageData)
    }
  }
  data <- data[,c(1:5,17)]
  return(data)
}

###embedded figure###############################################################

embeddedFigure <- function(i) {
  
  
  if (i=="Figure") {
    apiPrePage2 <- allData[(allData$unit=="2") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  for(j in allStudents){
    
    answers = data.frame('set'=rep(1:2,each=10),'complex'=rep(1:10,2),'simple'=c(c('a','b','a','b','b','a','a','a','b','b'),c('a','a','b','b','b','b','a','a','b','a')),stringsAsFactors=F)
    pageData <- apiPrePage2[apiPrePage2$user_id==j, ]
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "single-stim", ]
    # ASCII key codes 89=Y, 78=N
    pageData$trial = pageData$trial_index+1
    pageData$response = ifelse(pageData$key_press==89,'y','n')
    pageData$set = sapply(pageData$stimulus,function(r){substr(r,27,27)})
    pageData$simple = sapply(pageData$stimulus,function(r){substr(r,29,29)})
    pageData$complex = sapply(pageData$stimulus,function(r){gsub('\\.','',substr(r,78,79))})
    pageData$match = apply(pageData,1,function(r){ifelse(answers[answers$set==r['set'] & answers$complex==r['complex'],'simple']==r['simple'],'y','n')})
    pageData$correct = pageData$match==pageData$response
    pageData <- pageData[,c('trial',"rt", "response", 'set','simple','complex','match','correct')]
    pageData <- cbind(pageData, data.frame(user=rep(j,nrow(pageData))))
    data <- rbind(data,pageData)
  }
  data <- data[,c(1:3,7:9)]
  return(data)
}

###letter sets###################################################################

letterSets <- function(i) {
  
  
  apiPrePage3 <- allData[(allData$unit=="3") & (allData$phase=="pre"),]
  
  if (i=="Letter1") {
    
    apiPrePage3 <- apiPrePage3[as.Date(apiPrePage3$created) < as.Date("2017-01-01"),]
    
    data <- data.frame()
    
    
    for(j in allStudents){
      
      pageData <- apiPrePage3[apiPrePage3$user_id==j, ]
      if(nrow(pageData)==0) next
      pageData <- fromJSON(as.character(pageData$data[1]))
      pageData <- pageData[pageData$trial_type == "multi-choice" & pageData$internal_chunk_id == "0-0" & pageData$trial_index_globa > 2, ]
      pageData$correctanswer <- c(1, 4, 4, 3, 2, 3, 2, 4, 5, 2, 4, 3, 4, 2, 2, 5)
      pageData$studentcorrect <- ifelse(pageData$response == pageData$correctanswer, TRUE, FALSE)
      pageData$user <- j
      data <- rbind(data, pageData)
    }
    data <- data[,c(8,10:12)]
    
    
  }
  
  
  if (i=="Letter2"){
    
    apiPrePage3 <- apiPrePage3[as.Date(apiPrePage3$created) > as.Date("2017-01-01"),]
    
    data <- data.frame()
    
    for(j in allStudents){
      
      pageData <- apiPrePage3[apiPrePage3$user_id==j, ]
      
      if(nrow(pageData)==0) next
      pageData <- fromJSON(as.character(pageData$data[1]))
      pageData <- pageData[pageData$trial_type == "multi-choice" & pageData$internal_chunk_id == "0-0" & pageData$trial_index_globa > 2, ]
      pageData$correctanswer <- c(1,4,2,2,5,1,1,3,2,3,2,5,5,1,2)
      pageData$correct <- ifelse(pageData$response == pageData$correctanswer, TRUE, FALSE)
      pageData$user <- j
      data <- rbind(data, pageData)
    }
    data <- data[,c(2,8,10:12)]
  }
  return(data)
}

###vocab#######################################################################
vocab <- function(i) {
  
  
  apiPrePage4 <- allData[(allData$unit=="4") & (allData$phase=="pre"),]
  
  
  if (i=="Vocab1"){
    
    apiPrePage4 <- apiPrePage4[as.Date(apiPrePage4$created) < as.Date("2017-01-01"),]
    
    data <- data.frame()
    
    for(j in allStudents){
      
      pageData <- apiPrePage4[apiPrePage4$user_id==j, ]
      if(nrow(pageData)==0) next
      pageData <- fromJSON(as.character(pageData$data[1]))
      pageData <- pageData[pageData$trial_type == "multi-choice" & pageData$internal_chunk_id == "0-0" & pageData$trial_index_globa > 1, ]
      pageData$word <- c("airtight",  "peddle", "raider", "energetically", "implicate",  "legibleness", "laceration", "jollification", "willowy", "feline", "dispiritedly", "intricacy", "excerpt", "arrogance",  "exorbitance", "chef", "milestone", "chowder", "emancipator", "consultative", "emergence", "ignoramous", "calamitous", "incubate", "incessantness", "devitalize", "exonerate", "decadence", "ungainly", "pestilential")
      pageData$correctanswer <- c(3, 4, 2, 4, 1, 3, 1, 2, 1, 4, 4, 2, 2, 3, 1, 3, 1, 4, 2, 3, 4, 4, 3, 4, 2, 2, 4, 1, 3, 1) 
      pageData$studentcorrect <- ifelse(pageData$response == pageData$correctanswer, TRUE, FALSE)
      pageData$user <- j
      data <- rbind(data, pageData)
    }
    data <- data[,c(8,10:13)]
  }
  
  if (i=="Vocab2") {
    
    apiPrePage4 <- apiPrePage4[as.Date(apiPrePage4$created) > as.Date("2017-01-01"),]
    
    data <- data.frame()
    
    for(j in allStudents){
      
      pageData <- apiPrePage4[apiPrePage4$user_id==j, ]
      
      if(nrow(pageData)==0) next
      pageData <- fromJSON(as.character(pageData$data[1]))
      pageData <- pageData[pageData$trial_type == "multi-choice" & pageData$internal_chunk_id == "0-0" & pageData$trial_index_globa > 1, ]
      pageData$word <- c("mumble", "perspire", "gush", "massive", "feign",  "unwary", "veer", "orthodox", "stripling", "salubrious", "limpid", "procreate", "replete", "frieze",  "treacle", "ignominous", "abjure", "duress", "bayonet", "astound", "contamination", "amplify", "mural (pertaining to)", "hale", "meander", "burnish", "duplicity", "mundane", "deleterious", "nascent", "prolific",  "paroxysm", "antipodal", "acrimony", "lissome", "succinct")
      pageData$correctanswer <- c(1,2,2,4,1,3,1,1,4,5,3,4,1,3,5,4,3,5,4,3,4,2,5,4,5,1,2,1,1,5,1,4,4,5,4,2) 
      pageData$correct <- ifelse(pageData$response == pageData$correctanswer, TRUE, FALSE)
      pageData$user <- j
      data <- rbind(data, pageData)
    }
    data <- data[,c(2,8,10:13)]
  }
  return(data)
}

###add/subtract##################################################################

addSubtract <- function(i) {
  
  
  if (i=="AddSub") {
    apiPrePage5 <- allData[(allData$unit=="5") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  for(j in allStudents){
    
    pageData <- apiPrePage5[apiPrePage5$user_id==j, ]
    
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "survey-textline", ]
    
    pageData$response <- as.numeric(gsub("([-])|[[:punct:]]|Q0", "\\1", pageData$responses))
    
    names = names(pageData)
    index <- which(names == "0")
    names(pageData)[index] <-paste("stimulus")
    
    pageData$correctanswer <- sapply(pageData$stimulus,function(t){return(eval(parse(text=t)))})
    pageData$stimclean <- gsub("[[:punct:]]", "", pageData$stimulus)
    pageData$add1 <- substr(pageData$stimclean,1,2)
    pageData$add2 <- substr(pageData$stimclean,3,4)
    pageData$add1 = as.numeric(pageData$add1)
    pageData$add2 = as.numeric(pageData$add2)
    
    pageData$correct <- ifelse(is.na(pageData$response), FALSE,
                               ifelse(pageData$response == pageData$correctanswer, TRUE, FALSE))
    pageData$user <- j
    data <- rbind(data, pageData)
  }
  data <- data[,c(2,8,10:11,15:16)]
  return(data)
}

###markXY#######################################################################

markXY <- function(i) {
  
  
  if (i=="MarkXYI") {
    apiMarkXY <- allData[(allData$unit=="6") & (allData$phase=="pre"),]}
  
  if (i=="MarkXYII") {
    apiMarkXY <- allData[(allData$unit=="7") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  if (i=="MarkXYI" | i=="MarkXYII") {
    filteredApiMarkXY <- data.table(apiMarkXY[apiMarkXY$user_id %in% allStudents, ])
    filteredApiMarkXY = filteredApiMarkXY %>% 
      group_by(user_id) %>%
      arrange(desc(id)) %>% 
      slice(1L)
  }
  

  for (row in 1:nrow(filteredApiMarkXY)) {
    
    userData <- filteredApiMarkXY[row, ]
    j = userData$user_id
    
    if(nrow(userData)==0) next
    pageData <- fromJSON(as.character(userData$data[1]))
    pageData <- pageData[pageData$trial_type == "html", ]
    newData <- data.frame(pageData$trials)
    correct <- c()
    response.x <- c()
    response.y <- c()
    rt <- c()
    for(k in 1:nrow(newData)) {
      attemptsData <- unlist(newData$attempts[k])
      correct <- c(correct, attemptsData["correct"])
      response.x <- c(response.x, attemptsData["response.x"])
      response.y <- c(response.y, attemptsData["response.y"])
      rt <- c(rt, attemptsData["rt"])
    }
    newData$correct <- as.logical(correct)
    newData$response.x <- as.numeric(response.x)
    newData$response.y <- as.numeric(response.y)
    newData$rt <- as.numeric(rt)
    newData$user <- rep(j,nrow(pageData))
    newData <- newData[, c("stimulus","correct","response.x","response.y","rt", "user")]
    data <- bind_rows(data, newData)
    
  }
  
  data$correct <- as.logical(data$correct)
  data$rt <- as.numeric(data$rt)
  data$user <- as.character(data$user)
  ##convert em-dash to -
  data$stimulus <- iconv(data$stimulus, "", "ASCII", "byte")
  data$stimulus <- gsub("<e2><80><93>", "-", data$stimulus)
  ##increase "correct" threshold
  data$goal.x <- as.numeric(gsub(".*\\(|\\,.*", "", data$stimulus))
  data$goal.y <- as.numeric(gsub(".*\\,|\\).*", "", data$stimulus))
  data$correct <- ifelse((abs(data$goal.x-as.numeric(data$response.x)) < 0.041) & (abs(data$goal.y-as.numeric(data$response.y)) < 0.041), TRUE, FALSE)
  data <- data[,c(1:6)]
  
  return(data)
}


###mark theta####################################################################

markTheta <- function(i) {
  
  
  if (i=="MarkThetaI") {
    apiMarkTheta <- allData[(allData$unit=="8") & (allData$phase=="pre"),]}
  
  if (i=="MarkThetaII") {
    apiMarkTheta <- allData[(allData$unit=="9") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  
  if (i=="MarkThetaI" | i=="MarkThetaII") {
    filteredApiMarkTheta = data.table(apiMarkTheta[apiMarkTheta$user_id %in% allStudents, ])
    filteredApiMarkTheta = filteredApiMarkTheta %>% 
      group_by(user_id) %>%
      arrange(desc(id)) %>% 
      slice(1L)
  }
  

  for(row in 1:nrow(filteredApiMarkTheta)){
    
    pageData <- filteredApiMarkTheta[row, ]
    j = pageData$user_id
    
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "html", ]
    pageData <- as.data.frame(pageData$trials)
    
    correct <- c()
    response.theta <- c()
    rt <- c()
    for(k in 1:nrow(pageData)) {
      attemptsData <- unlist(pageData$attempts[k])
      correct <- c(correct, attemptsData["correct"])
      response.theta <- c(response.theta, attemptsData["response.theta"])
      rt <- c(rt, attemptsData["rt"])
    }
    pageData$correct <- correct
    pageData$response.theta <- as.numeric(response.theta)
    pageData$rt <- as.numeric(rt)
    pageData$user <- rep(j,nrow(pageData))
    pageData <- pageData[, c("stimulus","correct","response.theta", "rt", "user")]
    data <- rbind(data,pageData)
    
  }
  
  data$user <- as.character(data$user)
  
  return(data)
}

###trig concepts#################################################################


trigConcepts <- function(i) {
  
  if (i=="RTT") {
    apiPrePage10 <- allData[(allData$unit=="10") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  for(j in allStudents){
    pageData <- apiPrePage10[apiPrePage10$user_id==j, ]
    
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "survey-textline" & pageData$trial_index_global < 12, ]
    chunk1 <- unlist(fromJSON(pageData$responses[1]))
    chunk2 <- unlist(fromJSON(pageData$responses[2]))
    chunk3 <- unlist(fromJSON(pageData$responses[3]))
    chunk4 <- unlist(fromJSON(pageData$responses[4]))
    chunk5 <- unlist(fromJSON(pageData$responses[5]))
    chunk6 <- unlist(fromJSON(pageData$responses[6]))
    chunk7 <- unlist(fromJSON(pageData$responses[7]))
    chunk8 <- unlist(fromJSON(pageData$responses[8]))
    chunk9 <- unlist(fromJSON(pageData$responses[9]))
    chunk10 <- unlist(fromJSON(pageData$responses[10]))
    chunk11 <- unlist(fromJSON(pageData$responses[11]))
    questionlabel <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13","Q14","Q15","Q16","Q17","Q18","Q19", "Q20", "Q21", "Q22","Q23","Q24","Q25","Q26","Q27","Q28", "Q29", "Q30")
    answers <- c("4/5", "3/5", "4/3", "3/5", "8", "6", "10", "6", "8/10", "6/10", "8/6","6/10","1/2","sqrt(3)/2","-sqrt(3)/2","1/2","-sqrt(3)/2","-1/2","30","1/2","sqrt(3)/2","1/2","sqrt(3)/2","-sqrt(3)/2","1/2","-sqrt(3)/2","-1/2","30","1/2","sqrt(3)/2")
    answers2 <- c(.8,.6,1.333,.6,8,6,10,6,.8,.6,1.333,.6,0.5,0.87,-.87,0.5,-.87,-0.5,30,0.5,0.87,0.5,0.87,-.87,0.5,-.87,-0.5,30,0.5,0.87)
    answers3 <- c("4/5", "3/5","4/3","3/5", "8", "6","10","6", "4/5","3/5","4/3","3/5", "0.5/1","0.87/1","-0.87/1","0.5/1","-0.87/1","-0.5/1","30 degrees","0.5/1","0.87/1","0.5/1","0.87/1","-0.87/1","0.5/1","-0.87/1","-0.5/1","30 degrees","0.5/1","0.87/1")
    response <- c(gsub(" ", "", chunk1[1]), chunk1[2], chunk1[3], chunk1[4], chunk2[1], chunk2[2], chunk2[3], chunk2[4], chunk3[1], chunk3[2], chunk3[3], chunk3[4], chunk4[1], chunk4[2], chunk5[1], chunk5[2], chunk6[1], chunk6[2], chunk7[1], chunk7[2], chunk7[3], chunk8[1], chunk8[2], chunk9[1], chunk9[2], chunk10[1], chunk10[2], chunk11[1], chunk11[2], chunk11[3])
    correct <- ifelse(answers == response | answers2 == response |answers3 == response, TRUE, FALSE)
    rt <- c(pageData$rt[1], pageData$rt[1], pageData$rt[1], pageData$rt[1], pageData$rt[2], pageData$rt[2], pageData$rt[2], pageData$rt[2], pageData$rt[3], pageData$rt[3], pageData$rt[3],pageData$rt[3], pageData$rt[4], pageData$rt[4],pageData$rt[5], pageData$rt[5],pageData$rt[6], pageData$rt[6],pageData$rt[7], pageData$rt[7],pageData$rt[7],pageData$rt[8],pageData$rt[8],pageData$rt[9],pageData$rt[9],pageData$rt[10],pageData$rt[10],pageData$rt[11],pageData$rt[11],pageData$rt[11])
    newData <- data.frame(question=questionlabel, answer=answers, answer2=answers2, answer3=answers3, response=response, correct=correct, rt=rt, user=rep(j,length(answers)))
    data <- rbind(data, newData)
    
  }
  data$response2 <- as.character(data$response)
  data$response3 <- as.numeric(data$response2)
  data$correct2 <- ifelse(data$response2==data$answer | data$response2==data$answer2 | data$response2==data$answer3 |
                            abs(data$response3 - data$answer2) < 0.031, TRUE, FALSE)
  data$correct2 <- replace(data$correct2, is.na(data$correct2), FALSE)
  data <- data[,c(1:2,5,7:8,11)]
  names(data)[6] <- "correct"
  return(data)
}

###trig identities###############################################################

preTrigID <- function(i) {
  
  if (i=="TrigID") {
    apiPrePage11 <- allData[(allData$unit=="11") & (allData$phase=="pre"),]}
  
  data <- data.frame()
  
  for (j in allStudents) {
    
    pageData <- apiPrePage11[apiPrePage11$user_id==j, ]
    
    if(nrow(pageData)==0) next
    pageData <- fromJSON(as.character(pageData$data[1]))
    pageData <- pageData[pageData$trial_type == "multi-choice" & pageData$internal_chunk_id == "0-0", ]
    
    pageData$user <- j  
    pageData = pageData[!is.na(pageData$trial_type),]
    
    sin <- function(degree){
      round(base::sin(degree/180*pi),digits=4)
    }
    cos <- function(degree){
      round(base::cos(degree/180*pi),digits=4)
    }
    trigchoice <- function(choice,degree){
      degree=as.numeric(degree)
      switch(choice,'1'=sin(degree),'2'=-sin(degree),'3'=cos(degree),'4'=-cos(degree),-99)
    }
    vals = apply(pageData,1,function(r){return(trigchoice(r['response'],r['theta']))})
    
    pageData$delta = trimws(pageData$delta)
    trigstimulus <- function(r){
      s = paste0(r['func'],'(')
      if (r['order']==1) {
        if (r['xsign']=='-') {
          s = paste0(s,'-')
        }
        s = paste0(s, r['theta'])
        if (as.numeric(r['delta'])>=0) {
          s = paste0(s, '+')
        }
        s = paste0(s, r['delta'],')')
      } else {
        s = paste0(s, r['delta'], r['xsign'], r['theta'], ')')
      }
    }
    
    
    pageData$stimulus = apply(pageData,1,trigstimulus)
    
    pageData$correct = vals == sapply(pageData$stimulus,function(t){return(eval(parse(text=t)))})
    pageData$response = factor(pageData$response,levels=1:4,labels=c('sin','-sin','cos','-cos'))
    responsecombined <- paste0(pageData$response, "(", pageData$theta, ")")
    pageData$responsecombined <- responsecombined
    data <- rbind(data, pageData)
  }
  return(data[,c('user', 'trial_index','rt','response','responsecombined', 'func','xsign','delta','order','theta','stimulus','correct')])
}

##pretest results
prePage1 <- figureRotation("Rotate")
prePage1sum <-
  prePage1 %>%
  group_by(user) %>%
  summarise(FigureRotation = round(sum(correct)/20*100,1))
prePage1sum$user <- as.numeric(prePage1sum$user)


prePage2 <- embeddedFigure("Figure")
prePage2sum <-
  prePage2 %>%
  group_by(user) %>%
  summarise(EmbeddedFigure = round(sum(correct)/20*100,1))
prePage2sum$user <- as.numeric(prePage2sum$user)


prePage3 <- letterSets("Letter1")
prePage3sum <-
  prePage3 %>%
  group_by(user) %>%
  summarise(LetterSets = round(sum(studentcorrect)/16*100,1))
prePage3sum$user <- as.numeric(prePage3sum$user)
#replace percent correct with z scores
prePage3sum <- prePage3sum %>%
  mutate(LetterSets = scale(LetterSets))


prePage3new <- letterSets("Letter2")
prePage3newsum <-
  prePage3new %>%
  group_by(user) %>%
  summarise(LetterSets = round(sum(correct)/15*100,1))
prePage3newsum$user <- as.numeric(prePage3newsum$user)
#replace percent correct with z scores 
prePage3newsum <- prePage3newsum %>%
  mutate(LetterSets = scale(LetterSets))


prePage3Both <- rbind(prePage3sum,prePage3newsum)

prePage4 <- vocab("Vocab1")
prePage4sum <-
  prePage4 %>%
  group_by(user) %>%
  summarise(Vocab = round(sum(studentcorrect)/30*100,1))
prePage4sum$user <- as.numeric(prePage4sum$user)
#replace percent correct with z scores
prePage4sum <- prePage4sum %>%
  mutate(Vocab = scale(Vocab))


prePage4new <- vocab("Vocab2")
prePage4newsum <-
  prePage4new %>%
  group_by(user) %>%
  summarise(Vocab = round(sum(correct)/36*100,1))
prePage4newsum$user <- as.numeric(prePage4newsum$user)
#replace percent correct with z scores
prePage4newsum <- prePage4newsum %>%
  mutate(Vocab = scale(Vocab))


prePage4Both <- rbind(prePage4sum,prePage4newsum)

prePage5 <- addSubtract("AddSub")
prePage5sum <-
  prePage5 %>%
  group_by(user) %>%
  summarise(AddSubtract = round(sum(correct)/32*100,1))
prePage5sum$user <- as.numeric(prePage5sum$user)


prePage6 <- markXY("MarkXYI")
prePage6sum <-
  prePage6 %>%
  group_by(user) %>%
  summarise(MarkXYI = round(sum(correct)/10*100,1))
prePage6sum$user <- as.numeric(prePage6sum$user)


prePage7 <- markXY("MarkXYII")
prePage7sum <-
  prePage7 %>%
  group_by(user) %>%
  summarise(MarkXYII = round(sum(correct)/10*100,1))
prePage7sum$user <- as.numeric(prePage7sum$user)


prePage8 <- markTheta("MarkThetaI")
tmp = prePage8$correct
prePage8$correct = ifelse(tmp == "TRUE" | tmp == "FALSE", as.logical(tmp), as.logical(as.numeric(tmp)))
prePage8sum <-
  prePage8 %>%
  group_by(user) %>%
  summarise(MarkThetaI = round(sum(correct,na.rm=TRUE)/12*100,1))
prePage8sum$user <- as.numeric(prePage8sum$user)


prePage9 <- markTheta("MarkThetaII")
tmp2 = prePage9$correct
prePage9$correct = ifelse(tmp2 == "TRUE" | tmp2 == "FALSE", as.logical(tmp2), as.logical(as.numeric(tmp2)))
prePage9sum <-
  prePage9 %>%
  group_by(user) %>%
  summarise(MarkThetaII = round(sum(correct,na.rm=TRUE)/12*100,1))
prePage9sum$user <- as.numeric(prePage9sum$user)


prePage10 <- trigConcepts("RTT")


prePage10$correct[is.na(prePage10$response)] = NA

prePage10sum <- 
  prePage10 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct, na.rm=TRUE)*100,1))
prePage10sum$user <- as.numeric(prePage10sum$user)

##divide right triangle trig into 5 pages
prePage10part1 <- prePage10[prePage10$question=="Q1" | prePage10$question=="Q2" | prePage10$question=="Q3",]
prePage10part2 <- prePage10[prePage10$question=="Q5" | prePage10$question=="Q6" | prePage10$question=="Q7",]
prePage10part3 <- prePage10[prePage10$question=="Q9" | prePage10$question=="Q10" | prePage10$question=="Q11",]
prePage10part4 <- prePage10[prePage10$question=="Q13" | prePage10$question=="Q14" | prePage10$question=="Q15" | prePage10$question=="Q16" | prePage10$question=="Q17" | prePage10$question=="Q18" | prePage10$question=="Q19" | prePage10$question=="Q20" | prePage10$question=="Q21",]
prePage10part5 <- prePage10[prePage10$question=="Q22" | prePage10$question=="Q23" | prePage10$question=="Q24" | prePage10$question=="Q25" | prePage10$question=="Q26" | prePage10$question=="Q27" | prePage10$question=="Q28" | prePage10$question=="Q29" | prePage10$question=="Q30",]


prePage10part1 <- 
  prePage10part1 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct,na.rm=TRUE)*100,1))
colnames(prePage10part1)[2] <- "Right Triangle Trig I"
prePage10part1$user <- as.numeric(prePage10part1$user)


prePage10part2 <- 
  prePage10part2 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct,na.rm=TRUE)*100,1))
colnames(prePage10part2)[2] <- "Right Triangle Trig II"
prePage10part2$user <- as.numeric(prePage10part2$user)


prePage10part3 <- 
  prePage10part3 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct,na.rm=TRUE)*100,1))
colnames(prePage10part3)[2] <- "Right Triangle Trig III"
prePage10part3$user <- as.numeric(prePage10part3$user)


prePage10part4 <- 
  prePage10part4 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct,na.rm=TRUE)*100,1))
colnames(prePage10part4)[2] <- "Right Triangle Trig IV"
prePage10part4$user <- as.numeric(prePage10part4$user)


prePage10part5 <- 
  prePage10part5 %>%
  group_by(user) %>%
  summarise(PercentCorrect = round(mean(correct,na.rm=TRUE)*100,1))
colnames(prePage10part5)[2] <- "Right Triangle Trig V"
prePage10part5$user <- as.numeric(prePage10part5$user)


prePage11 <- preTrigID("TrigID")
prePage11sum <-
  prePage11 %>%
  group_by(user) %>%
  summarise(TrigIDs = round(mean(correct)*100,1))
prePage11sum$user <- as.numeric(prePage11sum$user)


pretestResults <- list(prePage1sum,prePage2sum,prePage3Both,prePage4Both,prePage5sum,prePage6sum,prePage7sum,prePage8sum,prePage9sum,prePage10part1,prePage10part2,prePage10part3,prePage10part4,prePage10part5,prePage11sum) %>%
  Reduce(function(dtf1,dtf2) inner_join(dtf1,dtf2,by="user"), .)



#read in prescreen
prescreen <- read.csv('PrescreenResultsFall16-Fall18.csv')


##remove some variables
prescreen <- prescreen[,c(2:14,20:21)]

##reorder remaining variables
prescreen <- prescreen[c("user","math1", "math2", "math3", "math4", "math5", "math6", "math7", "math8", "math9", "chairparts",  "spatialrel", "diagrams","howmanymath","mathposthigh")]

##rename variables (ga: general attitude; ma: math attitude; sa: spatial attitude)
colnames(prescreen) <- c("user","MA.GoodatMath", "MA.ImportantDoWell", "MA.NaturallyBetter", "MA.EffortImportant", "MA.UnderstandQuickly", "MA.ThinkCarefully", "MA.PlugInNum", "MA.PatternsOverCalc", "MA.ExcitingIntimidating", "SA.ChairParts", "SA.SpatialRel", "SA.Diagrams","HSMath","PostHSMath")

##combine results
allREPPrescreenPretestResults <- merge(pretestResults,prescreen,by="user")

##save as .csv
write.csv(allREPPrescreenPretestResults, "allREPPrescreenPretestResults.csv")

