
### # The script may not work on my laptop, because the folder conda, r-miniconda, and sometimes reticulate have some problems. 
## it is a long procedure, but the best option is to remove and reinstall all the folders. 
## it may als not work, try to remove the folders with r open and check if the folder is automaticaly recreated by r, then re-remove it
setwd("C:/Users/lc365c/OneDrive - University of Glasgow/Desktop/Preliminary Script")



#use_condaenv(condaenv = "r-reticulate", conda = "C:\Users\lc365c\AppData\Local\r-miniconda\envs\r-reticulate/_conda.exe")
#remove.packages("reticulate") 

#Sys.setenv(RETICULATE_PYTHON="C:/Program Files/WindowsApps/PythonSoftwareFoundation.Python.3.8_3.8.2800.0_x64__qbz5n2kfra8p0")
install.packages("reticulate", dep=T)

## If something is not working, a good idea may be to cancel the reticulate package in r or r-studio folder and re-install it
library(reticulate)

#use_python("C:/Program Files/WindowsApps/PythonSoftwareFoundation.Python.3.8_3.8.2800.0_x64__qbz5n2kfra8p0/python.exe")
#repl_python()
######## MASTODON API  #########
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
# HERE FOLLOW THE INSTRUCTIONS AND GO FOR MASTODON AND THE USER/LOCAL


#my_id <- "3I5KPnn8EA_OBJRpWJR-a0zEKu0NSHIrTaSf7eA4brs" ## this was for the first time i tried the script

#a=get_instance_general(instance = "mastodon.social") ## the first time i retrieved the id using this command, however, it seems non necessary 

#get_instance_activity(instance = "mastodon.social") ### shows the activity for the last three months 
#b=get_instance_trends(instance = "mastodon.social") ## the trending hashtags of the week

### get toots about some specific hastags
#b=get_timeline_hashtag(hashtag = "wildlife", instance = "mastodon.social", limit=200)
### THIS IS JUST AN EXMPLE, THE FINAL DATASET PREPARATION IS BELOW
bG=get_timeline_hashtag(hashtag = c("Glasgow"), instance = "mastodon.social", limit=300) ## may take a bit of time

#### to post something directly
# post_toot(status = "my first rtoot #rstats") ## this is to directly post a toot on mastodon
## my mastodon account is a real mess ###



### Script to analyse a pipeline of Mastodon data  ############
library(rtoot) 
library(dplyr) 
library(DT)
library(httr)
library(reticulate)
library(jsonlite)
rm(list = ls())
gc(reset=T)
## sometimes it worked better if the miniconda environment is created with this line first
torch <- import("torch") ## to install anaconda 

### installing the packages in the python environment 
# FIRST I REMOVED ALL THE R-MINICONDA FOLDER AND REINSTALLED EVERYTHING
py_install("transformers", pip=T)
#py_install("torch", pip=F,metod="virtualenv", conda="auto" )  #### THE SECOND TI E I DID NOT INSTALLED TORCH, AS IT SEEMS TO ALREADY BE INCLUDED IN TRANSFORMER
py_install("tensorflow") ### this work better without pip

py_install("torch", pip=T)
#py_install("PyTorch")

############
# Theoretically, the installation phase should be done only once. 
# However, I get some conflicts with other packages or virtual environments, that's why i have to manually cancel and reinstall everything


### importing the packages to the  R environment
transformers <- import("transformers") 
TFl <- import("tensorflow")
torch <- import('torch') 
# reticulate::py_install(c("torch"), pip = F)
PyTorch <- import('pytorch') 

# Retrieve/force initialization of Python
reticulate::py_config()
reticulate::py_available() ## phyton seems to be available 

### uploading the tokenization and the model now 
tokenizer <- transformers$AutoTokenizer$from_pretrained('cardiffnlp/twitter-roberta-base-sentiment') ## this is the roberta based tokenizen. Text should be transformed in "numbers" before being analysed
model     <- transformers$AutoModelForSequenceClassification$from_pretrained('cardiffnlp/twitter-roberta-base-sentiment') ## this is how we upload roberta model fro hugging face


### defining the three function to perform parallel work on the toots
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

#### HERE THE WORKFLOW after I have imported the data 
## the all workflow cam cope with almost 500 toots, more than that We will need more ram 

### ### select in what are we interested? an hastag maybe?? ### glasgow in this example
# IT SEEMS SMART TO FIRST GET GLASGOW AND THEN PARKS OR WILDLIFE. 
# check how can we retrieve multiple hash in a single request 
bG=get_timeline_hashtag(hashtag = c("Glasgow"), instance = "mastodon.social", limit=5000) ## the hastag is not case sensitive, Glasgow and glasgow gave the exactly same results

### This modify the retrieved toots using a html package
bG1=purrr::map_chr(bG$content[], ~ rvest::html_text2(rvest::read_html(.))) ### this convert all the data from http to text

### i then get some errors in evaluating roberta on tweets, it worked for few, but not for all in batch....
### I SUSPECT THE PROBLEM IS IN THE TEXT, IN FACT SOME TOOTS HAVE VERY LONG CONTENT 
### THEN REMOVE STOPWORDS AND PUNCTUATION AND STRANGE SYMBOLS
### EVENTUALLY ALSO REMOVE NON SIGNIFICANT TWEETS (THOSER REGARDING OTHER THINGS)
# check for the language, is it english?
## pre-process tweets ########
library(tm)


##### eliminating duplicated tweets 
bG1<-unique(bG1)  ### 3874-3765 109 duplicated elements  

#### for now i decided to took the non normalized (with punctuation and stop words) open to check for differences in results 

##### using python i found that there may be some very long posts, that i may not be aware of if I am not doing a loop 

df=as.data.frame(cbind(c("this is a fake toot"),c(0)))  ## initializing the loop
## create a dataframe of the toot and it's length
for (BG in bG1) {
  print (BG)
  print(nchar(BG))
  df=rbind(df,c(BG,as.numeric(nchar(BG))))
}  
df=df[-1,]
library(textcat)
df$lang<-textcat(df$V1)

### taking only the welsh English Scots etc 
table(df$lang) # how many languages are there? 
df$V2=as.numeric(df$V2) ## how long are toots?
summary((df$V2))
df=df[df$lang %in% c("english","scots"),] ### taking only english and scots 
df=df[df$V2 <=500,] ### taking only shortest toots that allow roberta working 
## almost 700 toots were not in english and 91 were longer than 500 
## THAT'S A REASONABLE LOSS
### checking 
table(df$lang) ## eng and scots 
summary((df$V2)) # short toots, no long computational intense things

library(tm)

#### CLEANING STOPWORDS AND PUNCTUATION 
df$V1=sapply(df$V1,removePunctuation) ### this remove the punctuation, but the tokenization still do not work on long batch. can i do a general for loop? eventually? 
## also removing the stopwords, I have now lost all the hastags, IS IT EASIER FOR ROBERTA TO WORK THAT WAY?? MAYBE CHECK ROBERTA DOCUMENTATION?
df$V1 <- removeWords(df$V1, stopwords("english"))
### THIS CUCK CAN BE DONE OR NOT, DEPENDING ON THE COMPUTATION NECESSITIES


#### taking only toots referring to parks 

a=df[grepl ("park", df$V1),] #### subsetting tweets regarding park 
# If for example i want to get parks or art or specific set of people 

#exp(scores)/sum(exp(scores))
## use previously evaluated function to elaborate the toots

tokenizer$model_max_length=400 ### with this set to 700 it work to handle 100 toots
### a tradeoff here, the more the length the more the memory allocated, the less the length the higer the probability of longer toots not being correctly analysed 

input_ids <- sapply(df[,"V1"] , function_tokenize)  ### This is to make a tokenized list the model can use (numbers) 
### tokenized all them but need still to analyze

out <- sapply(input_ids[] , function_model)  ### these are listed in a list of sentence and their result
## here a subset is better, more than 400 toots make R to badly  crash 


res<-lapply(out,function_open) ## open the tweets results 

res.mat=do.call(rbind.data.frame, res) ### matrixing it 
# the first probability is for Negative
# the second category is Neutral
# the third category is Positive
colnames(res.mat)=c("Negative","Neutral","Positive") ## give names to the columns 

apply(res.mat[,],2,summary) ### glasgow is suscitating generally neutral feelings, the positive are widely more than the negative 

### how many tweets do i have analysed?? 
dim(res.mat) ### 300 fine 
#### now word cloud with pre-elaborated data ######
library( wordcloud)
library( RColorBrewer)
library( tm)
library( dplyr)
#### bG2 is my dataset 
mytext <- Corpus (VectorSource(df$V1))


### preparing the dataset to wordclous
mytext <- mytext %>%
  tm_map (removeNumbers) %>%
  tm_map (removePunctuation) %>%
  tm_map (stripWhitespace)

mytext <- tm_map( mytext, content_transformer (tolower)) # put everything in lower case 
mytext <- tm_map (mytext, removeWords, stopwords ("english")) ## remove stopwords, they were already removed
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
           ,min.freq = 1 ,max.words=50 ,random.order=FALSE,
           rot.per=0.25, colors=brewer.pal (8, "Dark2")) ### ok it worked also with my fake dataset.



### OR MAYBE WITH SENTIMENTS ######


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



#### ANOTHER APPROACH IS NEEDED TO DO A WORDCLOUD WITH SENTIMENTS VALUES 
