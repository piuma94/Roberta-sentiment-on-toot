# Roberta-sentiment-on-toot 11-04-2023
The primary aim of this script is to retrieve and analyse with a sentiment analysis the toots associated with the word #Glasgow, to understand which are the general feelings of the sampled population regarding the city. 


This code is an R-based code, that allows using your mastodon profile (with username and PSW) to get toots regarding a certain hashtag. 
After getting the toots and converting them from an HTML format, remove long, duplicate or not in English toots. You can find how I suggest uploading the Roberta
algorithm from hugging face and using a python backend to extract sentiments from the toots. In the end, two datasets are created, 1) with all the analysed toots, their length and language and 2) a three-column dataset with their sentiment polarity values.

I finally insert the code to get a word cloud graph (without polarities) to understand the most common words associated with the selected hashtag. 
