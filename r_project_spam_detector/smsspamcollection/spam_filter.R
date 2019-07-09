dataset=read.csv("SMSSpamCollection.csv",sep="\t")
spam_dataset<-data.frame()
j<<-1
for(i in 1:length(dataset[,1]))
{
  if(dataset[i,1]=="spam")
  {spam_dataset[j,1]<-dataset[i,1]
  spam_dataset[j,2]<-dataset[i,2]
   
  j<<-j+1
  }}


#preprocessing


#data preprocessing
names(spam_dataset)[2]<-"comments"
mycorpus= Corpus(VectorSource(spam_dataset$comments))
#mycorpus[[1]]
as.character(mycorpus[[1]])

# Convert the text to lower case
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
as.character(mycorpus[[1]])

# Remove numbers
mycorpus <- tm_map(mycorpus, removeNumbers)
as.character(mycorpus[[1]])
# Remove english common stopwords
#mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
#as.character(mycorpus[[1]])

# Remove punctuations
mycorpus <- tm_map(mycorpus, removePunctuation)
as.character(mycorpus[[1]])
# Eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stripWhitespace)
as.character(mycorpus[[1]])














#onegram

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=1,max=1, delimiters = token_delim))
bitoken
#frequency table
words_with_freq <- data.frame(table(bitoken))
words_with_freq

names(words_with_freq)[1]<-"word"
names(words_with_freq)[2]<-"Freq"

words_with_freq<- words_with_freq[order(words_with_freq$Freq),]
words_with_freq

j<<-1
spam_sort_freq<-data.frame()
for(i in 1:length(words_with_freq[,1]))
{
  temp<-strsplit(as.character(words_with_freq[i,1]),split = "")
  if(length(temp[[1]])>=3)
  {spam_sort_freq[j,1]<-words_with_freq[i,1]
  spam_sort_freq[j,2]<-words_with_freq[i,2]
  j<<-j+1
  }}
names(spam_sort_freq)[1]<-"word"
names(spam_sort_freq)[2]<-"Freq"

a<-(length(spam_sort_freq[,1])-29)
b<-length(spam_sort_freq[,1])
spam_words<-spam_sort_freq[a:b,1]
spam_words










non_spam_dataset<-data.frame()
j<<-1
for(i in 1:length(dataset[,1]))
{
  if(dataset[i,1]!="spam")
  {non_spam_dataset[j,1]<-dataset[i,1]
  non_spam_dataset[j,2]<-dataset[i,2]
  
  j<<-j+1
  }}


#preprocessing


#data preprocessing
names(non_spam_dataset)[2]<-"comments"
mycorpus= Corpus(VectorSource(non_spam_dataset$comments))
#mycorpus[[1]]
as.character(mycorpus[[1]])

# Convert the text to lower case
mycorpus <- tm_map(mycorpus, content_transformer(tolower))
as.character(mycorpus[[1]])

# Remove numbers
mycorpus <- tm_map(mycorpus, removeNumbers)
as.character(mycorpus[[1]])
# Remove english common stopwords
#mycorpus <- tm_map(mycorpus, removeWords, stopwords("english"))
#as.character(mycorpus[[1]])

# Remove punctuations
mycorpus <- tm_map(mycorpus, removePunctuation)
as.character(mycorpus[[1]])
# Eliminate extra white spaces
mycorpus <- tm_map(mycorpus, stripWhitespace)
as.character(mycorpus[[1]])














#onegram

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=1,max=1, delimiters = token_delim))
bitoken
#frequency table
nspam_words_with_freq <- data.frame(table(bitoken))
nspam_words_with_freq
j<<-1
nspam_sort_freq<-data.frame()
for(i in 1:length(nspam_words_with_freq[,1]))
{
  temp<-strsplit(as.character(nspam_words_with_freq[i,1]),split = "")
  if(length(temp[[1]])>=3)
  {nspam_sort_freq[j,1]<-nspam_words_with_freq[i,1]
  nspam_sort_freq[j,2]<-nspam_words_with_freq[i,2]
  j<<-j+1
  }}
names(nspam_sort_freq)[1]<-"word"
names(nspam_sort_freq)[2]<-"Freq"
#sort according to names(alphabetical order)
nspam_sort_freq <- nspam_sort_freq[order(nspam_sort_freq$Freq),]
nspam_sort_freq


ind<-(length(nspam_sort_freq[,1])-19)
non_spam_words<-nspam_sort_freq[ind:length(nspam_sort_freq[,1]),1]





#set difference

for(i in 1:30)
{
  for(k in 1:20)
  {
    print("value")
    print(i)
    print(k)
    if(!is.na(spam_words[i]))
    if(spam_words[i]==non_spam_words[k])
    { print("enter")
      spam_words[i]<-"0"
    }
    
  }
  

}
keywords<-c()
j<<-1
for(i in 1:30)
{
 if(!is.na(spam_words[i])) 
  {keywords[j]<-as.character(spam_words[i])
  j<<-j+1
}}
length(keywords)





input<-read.csv("input.csv",sep = "\t")
para<-input[1,1]
para<-as.character(para)
words<-c()
words<-strsplit(para,split = " ")

words<-as.vector(words[[1]])

words<-tolower(words)

words
words<-str_remove(words,"[.,()*&$%@!\t]")
count<<-0
length(words)
for(i in 1:length(words))
{  
  if(count==5)
  {
    print("spam")
    break
  }
  for(k in 1:19)
  {  
    
    if(words[i]!="")
    {if(words[i]==keywords[k])
    {
      count<<-count+1
      
      break
    }}
    
    
  }
  
}
