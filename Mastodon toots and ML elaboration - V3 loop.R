
### HO CANCELLATO TUTTA LA CARTELLA CONDA E R-MINICONDA
## DOPO DICHE HO PROVATO A IMPORT TRANSFORMERS E MI HA REINSTALLATO TUTTA CONDA DA SOLO 
### E rifatto tutto il resto, molto easy
setwd("C:/Users/lc365c/OneDrive - University of Glasgow/Desktop/Preliminary Script")



#use_condaenv(condaenv = "r-reticulate", conda = "C:\Users\lc365c\AppData\Local\r-miniconda\envs\r-reticulate/_conda.exe")
#remove.packages("reticulate") 

#Sys.setenv(RETICULATE_PYTHON="C:/Program Files/WindowsApps/PythonSoftwareFoundation.Python.3.8_3.8.2800.0_x64__qbz5n2kfra8p0")
install.packages("reticulate", dep=T)

## If something is not working, a good idea may be to cancel the reticulate package in r or r-studio and re-install it
library(reticulate)

######## MASTODON API EXAMPLE #########
# install.packages("rtoot") # if needed
library(rtoot) 
library(dplyr) 
library(DT)
library(httr)
library(jsonlite)
rm(list = ls())
gc(reset=T)
#auth_setup(name = "account1") ## here choose mastodon and then the user choice

## I THINK MY ACCOUNT WAS FIRST ASSOCIATED WITH THIS EMAIL, BECAUSE IF I POST A TWEET IT WORK WELL
accounts <- search_accounts("luigicaopinna") ### the id
#my_id <- "3I5KPnn8EA_OBJRpWJR-a0zEKu0NSHIrTaSf7eA4brs" ## this was for the first time i tried the script

#a=get_instance_general(instance = "mastodon.social")

#get_instance_activity(instance = "mastodon.social") ### shows the activity for the last three months 
#b=get_instance_trends(instance = "mastodon.social") ## the trending hashtags of the week

### get toots about some specific hastags
b=get_timeline_hashtag(hashtag = "wildlife", instance = "mastodon.social", limit=200)
bG=get_timeline_hashtag(hashtag = c("Glasgow"), instance = "mastodon.social", limit=3000)

#### to post something directly
# post_toot(status = "my first rtoot #rstats") ## this is to directly do a toot on mastodon



### Script to analyse a pipeline of data  ############
library(rtoot) 
library(dplyr) 
library(DT)
library(httr)
library(reticulate)
library(jsonlite)
rm(list = ls())
gc(reset=T)
### these were the installing pipeline used
# FIRST I REMOVED ALL THE R-MINICONDA FOLDER AND REINSTALLED EVERYTHING#
#py_install("transformers", pip=T)
#py_install("torch", pip=F,metod="virtualenv", conda="auto" )  #### THE SECOND TI E I DID NOT INSTALLED TORCH, AS IT SEEMS TO ALREADY BE INCLUDED IN TRANSFORMER
#py_install("tensorflow", pip=T) ### this work better without pip
#reticulate::py_install(c("transformers")) 
#py_install("tensorflow")
#py_install("torch", pip=T)
#py_install("pillow", pip=T)

#py_install("PyTorch")

## uploading the ML algorithms ### from hugging face 
transformers <- import("transformers") ### importing the packages to the fake R environment
TFl <- import("tensorflow") ### importing the packages to the fake R environment
torch <- import('torch') ### importing the packages to the fake R environment
# reticulate::py_install(c("torch"), pip = F) ## that's a one time thing too 
#PyTorch <- import('pytorch') ### importing the packages to the fake R environment

# Retrieve/force initialization of Python
reticulate::py_config()
reticulate::py_available() ## phyton seems to be available 
### uploading the tokenization and the model now 
tokenizer <- transformers$AutoTokenizer$from_pretrained('cardiffnlp/twitter-roberta-base-sentiment')
model     <- transformers$AutoModelForSequenceClassification$from_pretrained('cardiffnlp/twitter-roberta-base-sentiment')
### IF THE INSTALLING AND STAFF IS NOT WORKING, PLEASE SAVE UPLOAD THIS ENVIRONMENT WITH EVERYTHING IN IT
#save.image(file='myEnv_Roberta.RData') ### even if my torch is not working, i guess i will still have the data and the models uploaded 
#load('myEnv_Roberta.RData')


#### it may be necessary to add some text cleaning ventually?? 
### definingt he three function to work on the lapply
function_tokenize<- function(x){
  tokenizer$encode(text=x,return_tensors="pt")
  
} ### function to tokenize
function_model<- function(x){
  model(x)$logits} ### function to model
function_open<- function(x){
  #out1=out[[1]]
  scores <- x$detach()$numpy()
  # scores ### prima era 0.9 negative e basta
  # Softmax transformation to get probabilities for Negative, Neutral, and Positive
  exp(scores)/sum(exp(scores))
} ### function to model

#### HERE THE WORKFLOW

### ### select in what are we interested? an hastag maybe?? ### glasgow in this example
bG=get_timeline_hashtag(hashtag = c("Glasgow"), instance = c("mastodon.social"), limit = 2000000,retryonratelimit=T) ## the hastag is not case sensitive, Glasgow and glasgow gave the exactly same results
bG2023=get_timeline_hashtag(hashtag = c("Glasgow"), instance = c("mastodon.social"),since_id="2023-01-01 00:00:01",  limit=20000) ## the hastag is not case sensitive, Glasgow and glasgow gave the exactly same results
### but if i take the newest toot of all ??
#110190389169501095 Tjis is the id of the last toot
bG_new=get_timeline_hashtag(hashtag = c("Glasgow"), instance = c("mastodon.social"), since_id=110190389169501095 , limit=20000) ## the hastag is not case sensitive, Glasgow and glasgow gave the exactly same results
### IT SEEMS TO WORK, IF I GET THE LAST ID THEY WILL GIVE ME THE NEW TOOTS
#format(bG$created_at, format = "%Y")

#### this then to retvieve all toots 
# Search for all toots containing the hashtag "#Glasgow"
#bG <- get_timeline_hashtag(hashtag = "Glasgow", instance = "mastodon.social", limit = 40,retryonratelimit=T)
#options(scipen=999)

# Keep iterating over search results until all toots have been retrieved
#while (length(bG) > 0) {
#  max_id <- min((bG$id))
  #as.numeric(max_id)
  # max_id <- min(bG$created_at)-1  ## questo al secondo giro mi da 0
  
#  more_results <- get_timeline_hashtag(hashtag = "Glasgow",verbose=T, instance = "mastodon.social", limit = 400, max_id = max_id,retryonratelimit=T)
  
 # if (length(more_results) == 0) {
#    break
#  }
#  bG <- rbind(bG, more_results)
#  print(dim(bG))
#  bG<-bG[!duplicated(bG$id),] #### eliminated 100 duplicated toots, 
#  
#  Sys.sleep(15)
#}


#### THE TWO METHODS RETRIEVE EXACTRLYTHE SAME NUMBER OF TOOTS
## USE THE FIRST THAT IS WAY EASIER AND SHORTED TO APPROACH 


## i stopped it manally as i get to the max of 
### before we could only download 4117 for Glasgow
#save(bG, file="bG_lots")




### just do a try with COVID 
#bG=get_timeline_hashtag(hashtag = c("COVID"), instance = c("mastodon.social"), limit = 200000) ## the hastag is not case sensitive, Glasgow and glasgow gave the exactly same results


### add argument , since_id="2023-01-01 00:00:01" TO GET TOOTS OLDER THAN 2023
### SINCE ID IS WORKING FOR THE ID, NOT FOR THE DATE, SELECTING A CERTYAIN DATE WILL BE MORE DIFFICULT,
### PAY ATTENTION, THE TOOT HAS SOME INFOS INSIDE: -date -language -if image is inluded - link to th figure -description of the figure.

### This modify the retrieved toots using a html package
bG1<-bG
bG1$content=as.data.frame(purrr::map_chr(bG$content[], ~ rvest::html_text2(rvest::read_html(.)))) ### this convert all the data from http to text

### i then get some errors in evaluating roberta on tweets, it worked for few, but not for all in batch....
### I SUSPECT THE PROBLEM IS IN THE TEXT 
### A GOOD IDEA MAY BE TO SEARCH OTHER HASTAGS IN THE TOOTS, IN ORDER TO SELECT FOR EXAMPLE THE PARKS
### THEN REMOVE STOPWORDS AND PUNCTUATION AND STRANGE SYMBOLS
### EVENTUALLY ALSO REMOVE NON SIGNIFICANT TWEETS (THOSER REGARDING OTHER THINGS)
## pre-process tweets ########
library(tm)

##### using python i found that there may be some very long posts, that i may not be aware of if I am not doing a loop 
#df=as.data.frame(cbind(c("this is a fake toot"),c(0)))

##### eliminating duplicated tweets 
## check the length of the unique toots 
a=(unique(bG1$content))
dim(a) ## 3851 in our case,let' check
rm(a)
length(unique(as.numeric(bG1$id)))
a<-table((bG1$id))
View(a)
### some many times repeated toots 
h<-bG1[bG1$id=="99712661257870115" ,]

bG1<-bG1[!duplicated(bG1$content),] #### eliminated 100 duplicated toots, 
### THIS WAY TO DEFINE DUPLICATES IS WORKING 
### FURTHER IMPROVEMENTS MAY BE TO ELIMINATE TWEET THAT ANSWER SOMETHING OR BASED ON THE DATE 
#### for now i decided to took the non normalized (with punctuation and stop words) open to check for differences in results 
bG1$bot<-rep(NA)
### FIRST TRY TO IDENTIFY BOTS ###
for (i in 1:dim(bG1)[1]) {
  try( bG1[ i,"bot"]  <- bG1[[19]][[i]][["bot"]], silent=T) ## they have already written if these are bots or not 

  print(i)
}
### NOW eliminating bots 

bG1<-bG1[bG1$bot==F,] #### eliminated other bots 3759-4023  264 toots from bots, not to bad 


#df=as.data.frame(cbind(c("this is a fake toot"),c("en"),c(0)))
### add the column with the nchar and the language
bG1$nChar<-rep(0,dim(bG1)[1]) ## this change as the number of dowloaded toots change
## language 
bG1$detLang<-rep("eng",dim(bG1)[1])
colnames(bG1)
for (i in 1:dim(bG1)[1] ) {
  print (bG1[i,"content"])
  print(nchar(bG1[i,"content"]))
  bG1[i,"nChar"]=nchar(bG1[i,"content"])
  
}  
library(textcat)
bG1$detLang<-sapply(bG1$content,textcat)

### taking only the welsh english scots and 
table(bG1$detLang)
table(bG1$language)

## corss table 
table(bG1$detLang,bG1$language) ## it seems like the majority agrre on english vs some sort of english idiom. For this trial i would say keep it as it is described by the mastodon social

## now filter for the longer toots 
bG1$nChar=as.numeric(bG1$nChar) ## be sure these are numbers 


summary((bG1$nChar)) ## where does the mean and other values are set 
hist (bG1$nChar) ### over 500 we will loss very few 
hist (bG1$nChar, xlim=c(0,5000)) ## zoom in 
hist (bG1$nChar,breaks = c(0,300,600,1000,2000,70000)) ## zoom in 
table((bG1$nChar)) ## where does the mean and other values are set 

## then subsetting 
bG1=bG1[bG1$language %in% c("en","en-gb"),] ### taking only english and scots 
bG1=bG1[bG1$nChar <=500,] ### taking only shortest toots that allow roberta working
### more or less arbitrary threshold, a tweet is max 280 char 
#### may be interesting to eliminate also very short tweets 
## some are really short 
bG1=bG1[bG1$nChar >=50,] ### taking those with more than 50 words 
## all have more than 50 char


### checking 
table(bG1$language) ## eng and en--gb 
summary((bG1$nChar)) # short toots, no more htan 500 

#### CLEANING STOPWORDS AND PUNCTUATION FACULTATIVE SCRIPT PART#
#bG1a=bG1
#bG1a$content=sapply(bG1$content,removePunctuation) ### this remove the punctuation, but the tokenization still do not work on long batch. can i do a general for loop? eventually? 
## also removing the stopwords, I have now lost all the hastags 
#bG1a$content <- removeWords(bG1a$content, stopwords("english"))  ### THIS CAN BE DONE, HOWEVER, I THINK WILL REDUCE ROBERTA UNDERSTANDING 
### THIS CUCK CAN BE DONE OR NOT, DEPENDING ON THE COMPUTATION NECESSITIES


#### taking only toots referring to parks 

bG1_p=bG1[grepl ("park", bG1$content),] #### subsetting tweets regarding park 

#exp(scores)/sum(exp(scores))
## use previously evaluated function to elaborate the toots




### EXTRACT 2023 TOOTS, 2022 AND SO ON If we want to check how the things are going, are the years different
BG_2023<-bG[format(bG$created_at, format = "%Y")==2023,]
## if i get the all without any specifics, we will lose a lot of data 
### check the time table. 
BG_2022<-bG[format(bG$created_at, format = "%Y")==2022,]

## It can be an idea to perform different analyses for each time period
### IMPLEMENTING IT AFTER THE LOOP TO EXTRACT THE RESULTS CAN ALSO BE A VERY GOOD IDEA
### WER CAN USE THE TOOT OR A CBIND TO REAGGREGATE RESULTS






##### THIS IS WORKING BUT ONLY FOR 200 TOOTS, LOOPING IT WITH THE LAPPLY ISDIDE MAY GIVE BETTER RESULTS?? 
res.mat1=c(0,0,0)
names(res.mat1)=c("Negative","Neutral","Positive")

#bG1=bG1[,]
### THIS ELABORATE 100 TOOTS AT A TIME, TO THEN RBIND THE RESULTS 
tokenizer$model_max_length=500 ### with this set to 700 it work to handle 100 toots

for (i in seq(1,dim(bG1)[1],by=100)) {
  
  if (i+99>dim(bG1)[1]) {
    a=list(as.vector(bG1[i:(dim(bG1)[1]),"content"]))
    print("perform last few toots")
  }else{
    a=list(as.vector(bG1[i:(i+99),"content"]))
  cat(paste("Perform toots from",i,"to",i+99 ))
  
}
  #a=list(as.vector(bG1[i:(i+99),"content"]))
input_ids <- sapply(a[[1]][["content"]][["purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))"]], function_tokenize)  ### This is to make a list of 
### takenized all them but need still to analyze

out <- sapply(input_ids[] , function_model)  ### this are listed in a list of sentence and their result

# the first probability is for Negative
res<-lapply(out,function_open)

res.mat=do.call(rbind.data.frame, res) ### matrixing it
rm(out)
rm(res)
# the first probability is for Negative
# the second category is Neutral
# the third category is Positive
colnames(res.mat)=c("Negative","Neutral","Positive")

print(apply(res.mat[,],2,summary)) ### glasgow is suscitating generally neutral feelings, the positive are widely more than the negative 

### how many tweets do i analysed?? 
print(dim(res.mat)) ### 300 fine 
res.mat1<-rbind(res.mat1,res.mat)
print(dim(res.mat1)) ### 300 fine 
gc(reset=T)
unlink(paste0(normalizePath(tempdir()), "/", dir(tempdir())), recursive = TRUE) ### eliminate temporary files

}

res.mat1= res.mat1[-1,]
res.mat1<-cbind(res.mat1,bG1$created_at,bG1$id, bG1$content, bG1$nChar) ### bG1$account $user_id,bG1$User_name
res.mat1$user_id<-rep(NA)
res.mat1$user_name<-rep(NA)
res.mat1$Mediatype<-rep(NA)
res.mat1$Media_descr<-rep(NA)
res.mat1$Media_link<-rep(NA)

for (i in 1:dim(bG1)[[1]]) {
  
  try(res.mat1[i,"user_name"]<- bG1$account[[i]]$username, silent=F)
  try(res.mat1[i,"user_id"]<- bG1$account[[i]]$id, silent=F)
  try(res.mat1[i,"Mediatype"]<- bG1$media_attachments[[i]]$type[1], silent=F)
  try(res.mat1[i,"Media_descr"]<- bG1$media_attachments[[i]]$description[1], silent=F)
  try(res.mat1[i,"Media_link"]<- bG1$media_attachments[[i]]$url[1], silent=F)
  
}

#### add image link and image description 

save(res.mat1, file="mastodon sentiment.Rda")

####  if i successfully import the image to text pipeline, i can perform a new image classification 

#### Now Continue the analyses with some identifyed themes ###########
load("mastodon sentiment.Rda")
res.mat1$year=format(res.mat1$`bG1$created_at`, format = "%Y") ### adding the year
res.mat1_p=res.mat1[grepl ("park|Parks", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 
res.mat1_pol=res.mat1[grepl ("pollinator|pollinators|Pollinator|Pollinators|Wildlife|wildlife", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 
#### Other identifyed themes, potentially interesting for WS1-2

### climate crisis live more sustainable WS2
res.mat1_cc=res.mat1[grepl ("Climate|climate|climatecrisis|climateCrisis|sustainable|Sustainable", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 

#### best things in the parks WS2
res.mat1_p=res.mat1[grepl ("park|parks", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 

### uni-city relationship to direct susitanible WS2 
res.mat1_uni=res.mat1[grepl ("University|UoG|GlasgowUNI", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 

### gender pay gap 
res.mat1_gender=res.mat1[grepl ("gender|paygap", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 
## not much working
### electricity 
res.mat1_fuel=res.mat1[grepl ("cold|electirc|heating|costofliving", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 

### public transport and public transport satisfaction 
res.mat1_transp=res.mat1[grepl ("bustopsbustops|centralstation|firstbus|publictransport|underground|subway|busses|clockwork|scotrail", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 


### before doing the images, can we get a general trend for our parks ?
#### now create a dataframe with all explored topics and values 
##parks
park=cbind(theme="park",res.mat1_p %>%
  group_by(year) %>%
  summarize(Min_neg = min(Negative), Max_neg = max(Negative), Median_neg = median(Negative),sd_neg = sd(Negative), Min_pos = min(Positive), Max_pos = max(Positive), Median_pos = median(Positive),sd_pos = sd(Positive),Min_neu = min(Neutral), Max_neu = max(Neutral), Median_neu = median(Neutral),sd_neu = sd(Neutral)))

## pollinator and wildlife
res.mat1_pol%>%
  group_by(year) %>%
  summarize(Min_neg = min(Negative), Max_neg = max(Negative), Median_neg = median(Negative),sd_neg = sd(Negative), Min_pos = min(Positive), Max_pos = max(Positive), Median_pos = median(Positive),sd_pos = sd(Positive),Min_neu = min(Neutral), Max_neu = max(Neutral), Median_neu = median(Neutral),sd_neu = sd(Neutral))

### uni 
## pollinator and wildlife
res.mat1_uni%>%
  group_by(year) %>%
  summarize(Min_neg = min(Negative), Max_neg = max(Negative), Median_neg = median(Negative),sd_neg = sd(Negative), Min_pos = min(Positive), Max_pos = max(Positive), Median_pos = median(Positive),sd_pos = sd(Positive),Min_neu = min(Neutral), Max_neu = max(Neutral), Median_neu = median(Neutral),sd_neu = sd(Neutral))


library(dplyr)
 plot(res.mat1_p %>%
  group_by(year) %>%
  #summarize(count = n()) %>%
  summarize(Mean = mean(Negative)), col="red" , pch=20, cex=3, ylim=c(0,1), xlim=c(2022,2023))

 pos=res.mat1_p %>%
   group_by(year) %>%
   #summarize(count = n()) %>%
   summarize(Mean = mean(Positive))
 
 points(pos$Mean~pos$year ,col="green" , pch=20, cex=3)
 
 neut=res.mat1_p %>%
   group_by(year) %>%
   #summarize(count = n()) %>%
   summarize(Mean = mean(Neutral))
 
 points(neut$Mean~neut$year ,col="grey" , pch=20, cex=3)
 
 
 ### however numerosities are different 
 table(res.mat1_p$year)
 ### generally trusting only 2022 on 
 
#plot(res.mat1_p[format(res.mat1$`bG1$created_at`, format = "%Y")),"Positive"] ~format(res.mat1$`bG1$created_at`, format = "%Y"))


#format(bG$created_at, format = "%Y")
 
############### chat GPT script for LDA ########
 library(tm)
 library(topicmodels)
 
 ### or only about parks
 #res.mat1=res.mat1[grepl ("park|Parks", res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`),] #### subsetting tweets regarding park 
 
 # Create a corpus of the tweets
 corpus <- Corpus(VectorSource(res.mat1$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))` ))
 
 # Perform preprocessing on the corpus
 corpus <- tm_map(corpus, tolower) # Convert to lowercase
 corpus <- tm_map(corpus, removePunctuation) # Remove punctuation
 corpus <- tm_map(corpus, removeNumbers) # Remove numbers
 corpus <- tm_map(corpus, removeWords, stopwords("english")) # Remove stopwords
 corpus <- tm_map(corpus, stripWhitespace) # Remove extra whitespace
 
 ##### THIS IS A FIRST TRY, HOWEVER, I CAN ALSO THING OF FIRST DIVIDE BY ARGUMENTS (BIODIVERSITY, PARKS, FOOD) 
 ####  AND THEN DECIDE WHAT ARE THE TYPICAL ARGUMENTS 
 
 #### implementation from internet 
 ## more infos here https://ladal.edu.au/topicmodels.html
 #######
 ## JUST A TRY 
 # compute document term matrix with terms >= minimumFrequency
 #minimumFrequency <- 5
 ##DTM <- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
 # have a look at the number of documents and terms in the matrix
# dim(DTM)
 # due to vocabulary pruning, we have empty rows in our DTM
 # LDA does not like this. So we remove those docs from the
 # DTM and the metadata
 #sel_idx <- slam::row_sums(DTM) > 0
 #DTM <- DTM[sel_idx, ]
 #textdata <- textdata[sel_idx, ]
 # number of topics
 #K <- 20
 # set random number generator seed
 #set.seed(9161)
 # compute the LDA model, inference via 1000 iterations of Gibbs sampling
 #topicModel <- LDA(DTM, K, method="Gibbs", control=list(iter = 500, verbose = 25)) 
 #terms(topicModel, 10)
 ## CAN I GET BACK TO TWEETS ??
 #exampleIds <- c(2, 100, 200)
 #lapply(corpus[exampleIds], as.character)
 ########
 # Convert the corpus to a document-term matrix
 dtm <- DocumentTermMatrix(corpus)
 
 # Perform LDA topic modeling on the document-term matrix
 lda <- LDA(dtm, k = 10, control = list(seed = 1234))
 
 # Print the topics and their top words
 terms(lda, 10)
 
 
 
 ###### as i need to do it many times 
 ### WRITE A FUNCTION
 preprocess_corpus <- function(text) {
   corpus <- Corpus(VectorSource(text))
   corpus <- tm_map(corpus, tolower)
   corpus <- tm_map(corpus, removePunctuation)
   corpus <- tm_map(corpus, removeNumbers)
   corpus <- tm_map(corpus, removeWords, stopwords("english"))
   corpus <- tm_map(corpus, stripWhitespace)
   dtm=DocumentTermMatrix(corpus)
   lda <- LDA(dtm, k = 10, control = list(seed = 1234))
   
   return(lda)
 } ### this function restitute the main topics of the LDA analyses
 
 
 #### performing LDA in one shot
 
a= preprocess_corpus(res.mat1_p$`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`)
terms(a, 10) ### for parks 
# THIS RAPYDLY GAVE THE MOST USED WORDS FOR EACH TOPIC
top5termsPerTopic <- terms(a, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
## mean topics for parks images 
a= preprocess_corpus(na.omit(res.mat1_p$Media_descr))
terms(a, 10)


a= preprocess_corpus(res.mat1_uni $`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`)
terms(a, 10) ## for uni 
## mean topics for uni images 
a= preprocess_corpus(na.omit(res.mat1_uni$Media_descr))
terms(a, 10)


 ### pollinators

a= preprocess_corpus(res.mat1_pol $`purrr::map_chr(bG$content[], ~rvest::html_text2(rvest::read_html(.)))`)
terms(a, 10) ## for uni 
## mean topics for uni images 
a= preprocess_corpus(na.omit(res.mat1_pol$Media_descr))
terms(a, 10)

 
#### now word cloud with pre-elaborated data ######
library( wordcloud)
library( RColorBrewer)
library( tm)
library( dplyr)
#### bG2 is my dataset 
mytext <- Corpus (VectorSource( bG1$content))


mytext <- mytext %>%
  tm_map (removeNumbers) %>%
  tm_map (removePunctuation) %>%
  tm_map (stripWhitespace)

mytext <- tm_map( mytext, content_transformer (tolower))
mytext <- tm_map (mytext, removeWords, stopwords ("english"))
# create a dataframe containing each word in your first column
# and their frequency in the second column.
mytextdata <- TermDocumentMatrix (mytext)
mtdmatrix <- as.matrix (mytextdata)
mywords <- sort( rowSums( mtdmatrix ),decreasing=TRUE)
mywordsdf <- data.frame (word = names (mywords ),freq=mywords)
set.seed( 412 )# for reproducibility

# this command ensures that the same word cloud is generated each time,
# the number in brackets is arbitrary, and can be any number that you would like to choose.
# Produce the wordcloud
# The arguments here use the 'words', the frequencies of the
#words, the minimum frequency of words to be included, the
#maximum number of words to be included, a non-random order
#indicates that words should be decreasing in size relative to
#frequency, the proportion of words to be rotated 90 degrees
#(rot.per), and a colour chart for the words
wordcloud (words = mywordsdf$word, freq = mywordsdf$freq
           ,min.freq = 1 ,max.words=100 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2")) ### ok it worked also with my fake dataset.



### OR MAYBE WITH SENTIMENTS 


### THERE IS NOT ENOUCH OVERLAPP, THAT'S WHY WE ARE NOT ABLE TO DO THE WORDCLOUD.
## ALSO BECAUSE I DO NOT UNDERSTAND IT PROPERLY
### sentiment 
library(textdata)
library(tidytext)

bing <- get_sentiments("bing")# %>% ## bing is a lexicon definition
 # filter(sentiment == "positive")


afinn <- get_sentiments("afinn")# %>% ## afinn is a lexicon definition
#  filter(value>0)   ## but uses different strenght to negative or positive values

### intersect with bing vocabulary
intersect(mywordsdf$word[1:100], bing$word) ### only 4 words can be associate with positive feelings 
### intersect with bing vocabulary

intersect(mywordsdf$word[1:100], afinn$word) ### only 4 words can be associate with positive feelings 

