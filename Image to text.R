

### Image to text tensorflow   ########
### DATA PREP
library(reticulate)
library(tensorflow)
library(keras)
#install.packages("huggingface")
#library(huggingface)

#reticulate::py_install(c("tensorflow"), pip = F) ## that's a one time thing too 
### this should be done to allow transformers installation 
#library(reticulate)
#py_install("pillow",pip=F, conda="auto")

## uploading the ML algorithms ### from hugging face 
#reticulate::py_install(c("transformers")) 
py_discover_config()

#pillow <- import("pillow") ### this finally work !!

transformers <- import("transformers") ### importing the packages to the fake R environment
TFl <- import("tensorflow") ### importing the packages to the fake R environment
torch <- import('torch') ### importing the packages to the fake R environment
#PyTorch <- import('pytorch') ### importing the packages to the fake R environment
# Retrieve/force initialization of Python
reticulate::py_config()
reticulate::py_available() ## phyton seems to be available 
### uploading the tokenization and the model now 

### FIRST AVAILBLE ALGORITHM
image_to_text = transformers$pipeline ("image-to-text", model="nlpconnect/vit-gpt2-image-captioning")
image_to_text1 =transformers$pipeline("image-to-text",model="ydshieh/vit-gpt2-coco-en")

image_to_text2 =transformers$pipeline("image-to-text",model="microsoft/git-base-coco")
## some examples 
image_to_text( "http://images.cocodataset.org/val2017/000000039769.jpg")
image_to_text1( "http://images.cocodataset.org/val2017/000000039769.jpg")
image_to_text2( "http://images.cocodataset.org/val2017/000000039769.jpg")
### from home they all work 


image_to_text("https://ankur3107.github.io/assets/images/image-captioning-example.png") ###THIS IS WORKING
image_to_text1("https://ankur3107.github.io/assets/images/image-captioning-example.png") ###THIS IS WORKING
image_to_text2("https://ankur3107.github.io/assets/images/image-captioning-example.png") ###THIS IS WORKING

## C:\Users\luigi\OneDrive - Universita degli Studi Roma Tre\Foto Luigi & Bunny- backup 08-12-2022 - Copy
image_to_text1("C:/Users/luigi/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/BD8DD2E4-99BC-4F03-A6B6-689339661DDF_1_105_c.jpeg")
image_to_text("C:/Users/luigi/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/BD8DD2E4-99BC-4F03-A6B6-689339661DDF_1_105_c.jpeg")
image_to_text2("C:/Users/luigi/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/BD8DD2E4-99BC-4F03-A6B6-689339661DDF_1_105_c.jpeg")

image_to_text1("C:/Users/lc365c/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/094CFBC9-92BE-402C-AE3F-F589E4FB538A_1_106_c.jpeg")
image_to_text("C:/Users/lc365c/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/094CFBC9-92BE-402C-AE3F-F589E4FB538A_1_106_c.jpeg")

## get the files in a folder 
#folder_path <- "C:/Users/lc365c/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022 - Copy"
folder_path <- "C:/Users/luigi/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022 - Copy"

###### performin image-to-text in a loop ######
# Get the list of files in the folder
file_list <- list.files(folder_path)
### analyse all images and convert to text
a=lapply(paste(folder_path,file_list[], sep="/"),image_to_text) ### create a list of images to analyse in the folder 
### extract the converted text 
a_ext=rep("a", length(a))
for (i in 1:length(a)) {
  
  a_ext[i]=a[[i]][[1]][["generated_text"]]
}

a_ext=as.data.frame(cbind(file_list,a_ext))
## this was evaluated on my all pics with Claudia for holydais 
save(a_ext,file="image-to-text all pics.Rda")
## check how it ddescribes a picture to my results on the screen of the laptop
a_ext[a_ext=="2D112332-C1C9-43E7-A6CA-B0F9B87C41A3_1_105_c.jpeg",]
load("image-to-text all pics.Rda")

### extract the most typical topics from our image-to-text ############
###### as i need to do it many times
library(tm)
library(topicmodels)

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
} 

a= preprocess_corpus(a_ext$a_ext)
terms(a, 10) ### for parks 

topicmodels::get_topics(a, 10)
top5termsPerTopic <- terms(a, 5)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")
### the main patterns in my pictures are: 
## working (screen, computer, pictures) holydays (mountain, beach, posing people) eating with dogs Mountains
### unbelievably dog is not a topic

### extract the wordcloud of our image-to-text ############
library( wordcloud)
library( RColorBrewer)
library( tm)
library( dplyr)

# create a dataframe containing each word in your first column
# and their frequency in the second column.
corpus <- Corpus(VectorSource(a_ext$a_ext))
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

mytextdata <- TermDocumentMatrix (corpus)
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




######## Another try image classification #############
library(jpeg)
#processor=transformers$ViTImageProcessor$from_pretrained('google/vit-base-patch16-224')
#model=transformers$ViTImageProcessor$from_pretrained('google/vit-base-patch16-224')

 ### google model
processor = transformers$ViTImageProcessor$from_pretrained('google/vit-base-patch16-224')
model = transformers$ViTForImageClassification$from_pretrained('google/vit-base-patch16-224')


image="C:/Users/luigi/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022/BD8DD2E4-99BC-4F03-A6B6-689339661DDF_1_105_c.jpeg"
img <- readJPEG("C:/Users/lc365c/OneDrive - Universita degli Studi Roma Tre/Foto Luigi & Bunny- backup 08-12-2022 - Copy/BD8DD2E4-99BC-4F03-A6B6-689339661DDF_1_105_c.jpeg")
#img <- readJPEG("http://images.cocodataset.org/val2017/000000039769.jpg")
inputs = list(processor(images=img, return_tensors="pt"))
#inputs = processor(images=img, return_tensors="pt")
#inputs <- list(processor(images=img, return_tensors="pt"))

outputs <- do.call(model, inputs)

#outputs = model((inputs))
#logits = outputs[[pop.items]]
logits = outputs$items

logits = outputs$data$pixel_values$logit
logits = outputs$data$pixel_values$argmax
# extract the logits and predicted class index
logits <- outputs$logits
predicted_class_idx <- torch_argmax(logits, dim = -1)

logits = outputs.logits
# model predicts one of the 1000 ImageNet classes
predicted_class_idx = logits$argmax#(-1).item()
print("Predicted class:", model$config$id2label[predicted_class_idx])
model.config.id2label
model$
#inputs <- list(processor(image = image, return_tensors = "pt"))

# Pass the inputs to the model using the named list syntax
outputs <- model$predict(inputs = inputs)

# Extract the logits from the model outputs
logits <- outputs$logits


outputs <- tensorflow::tf$nn$softmax(do.call(model, inputs)$logits)
