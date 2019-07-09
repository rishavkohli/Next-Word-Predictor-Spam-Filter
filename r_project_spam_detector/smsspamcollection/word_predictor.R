dataset=read.csv("SMSSpamCollection.csv",sep="\t")
#install.packages('tm')
library(tm)
library(ggplot2)
options(java.home="E:\\jdk1.8.0_102")
library(rJava)
#install.packages('RWeka')
library(RWeka)
#install.packages('wordcloud')
library(wordcloud)
library(stringr)
install.packages("pracma")
library(pracma)

dataset=dataset[,1:2]
#data preprocessing

mycorpus= Corpus(VectorSource(dataset$Comments))
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

# Bigrams 
minfreq_bigram<-2

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=2,max=2, delimiters = token_delim))
bitoken
#frequency table
two_word <- data.frame(table(bitoken))
two_word
#sort according to names(alphabetical order)
sort_two <- two_word[order(two_word$bitoken,decreasing=FALSE),]
sort_two
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Set1"),max.words=250)


#breaking words for faster access

word_predict_two<-data.frame('frequency'<-sort_two$Freq)
for(i in 1:length(sort_two$bitoken))
{ temp<-as.character(sort_two[i,1])
temp<-strsplit(temp, split=' ')
  word_predict_two[i,2]<-temp[[1]][1]
  word_predict_two[i,3]<-temp[[1]][2]
}

names(word_predict_two)[2]<-"input"
names(word_predict_two)[3]<-"output"

#indexing
index_two_word<-data.frame("alphabets"=letters)
n=0

for(i in 1:length(word_predict_two[,1]))
{  temp<-strsplit(word_predict_two[i,2],split = '')
   

if(n==0 && temp[[1]][1]=='a')
   { 
     index_two_word[1,2]<-i
     n=1
   }


if(n==1 && temp[[1]][1]=='b')
{ index_two_word[1,3]<-i-1
  index_two_word[2,2]<-i
  n=2
}


if(n==2 && temp[[1]][1]=='c')
{ index_two_word[2,3]<-i-1
  index_two_word[3,2]<-i
  n=3
}

if(n==3 && temp[[1]][1]=='d')
{ index_two_word[3,3]<-i-1
  index_two_word[4,2]<-i
  n=4
}

if(n==4 && temp[[1]][1]=='e')
{ index_two_word[4,3]<-i-1
  index_two_word[5,2]<-i
  n=5
}

if(n==5 && temp[[1]][1]=='f')
{ index_two_word[5,3]<-i-1
  index_two_word[6,2]<-i
  n=6
}


if(n==6 && temp[[1]][1]=='g')
{ index_two_word[6,3]<-i-1
  index_two_word[7,2]<-i
  n=7
}

if(n==7 && temp[[1]][1]=='h')
{ index_two_word[7,3]<-i-1
  index_two_word[8,2]<-i
  n=8
}

if(n==8 && temp[[1]][1]=='i')
{ index_two_word[8,3]<-i-1
  index_two_word[9,2]<-i
  n=9
}

if(n==9 && temp[[1]][1]=='j')
{ index_two_word[9,3]<-i-1
  index_two_word[10,2]<-i
  n=10
} 

if(n==10 && temp[[1]][1]=='k')
{ index_two_word[10,3]<-i-1
  index_two_word[11,2]<-i
  n=11
}

if(n==11 && temp[[1]][1]=='l')
{ index_two_word[11,3]<-i-1
  index_two_word[12,2]<-i
  n=12
}

if(n==12 && temp[[1]][1]=='m')
{ index_two_word[12,3]<-i-1
  index_two_word[13,2]<-i
  n=13
}

if(n==13 && temp[[1]][1]=='n')
{ index_two_word[13,3]<-i-1
  index_two_word[14,2]<-i
  n=14
}

if(n==14 && temp[[1]][1]=='o')
{ index_two_word[14,3]<-i-1
  index_two_word[15,2]<-i
  n=15
}

if(n==15 && temp[[1]][1]=='p')
{ index_two_word[15,3]<-i-1
  index_two_word[16,2]<-i
  n=16
}

if(n==16 && temp[[1]][1]=='q')
{ index_two_word[16,3]<-i-1
  index_two_word[17,2]<-i
  n=17
}

if(n==17 && temp[[1]][1]=='r')
{ index_two_word[17,3]<-i-1
  index_two_word[18,2]<-i
  n=18
}

if(n==18 && temp[[1]][1]=='s')
{ index_two_word[18,3]<-i-1
  index_two_word[19,2]<-i
  n=19
}

if(n==19 && temp[[1]][1]=='t')
{ index_two_word[19,3]<-i-1
  index_two_word[20,2]<-i
  n=20
}

if(n==20 && temp[[1]][1]=='u')
{ index_two_word[20,3]<-i-1
  index_two_word[21,2]<-i
  n=21
}

if(n==21 && temp[[1]][1]=='v')
{ index_two_word[21,3]<-i-1
  index_two_word[22,2]<-i
  n=22
}

if(n==22 && temp[[1]][1]=='w')
{ index_two_word[22,3]<-i-1
  index_two_word[23,2]<-i
  n=23
}

if(n==23 && temp[[1]][1]=='x')
{ index_two_word[23,3]<-i-1
  index_two_word[24,2]<-i
  n=24
}

if(n==24 && temp[[1]][1]=='y')
{ index_two_word[24,3]<-i-1
  index_two_word[25,2]<-i
  n=25
}

if(n==25 && temp[[1]][1]=='z')
{ index_two_word[25,3]<-i-1
  index_two_word[26,2]<-i
  n=26
}
}


index_two_word[26,3]<-length(word_predict_two[,1])








word_predict_two




# trigrams 
minfreq_bigram<-3

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=3,max=3, delimiters = token_delim))
bitoken
#frequency table
three_word <- data.frame(table(bitoken))
three_word
#sort according to names(alphabetical order)
sort_two <- three_word[order(three_word$bitoken,decreasing=FALSE),]
sort_two
wordcloud(sort_two$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Set1"),max.words=250)


#breaking words for faster access

word_predict_three<-data.frame('frequency'<-sort_two$Freq)
for(i in 1:length(sort_two$bitoken))
{ temp<-as.character(sort_two[i,1])
temp<-strsplit(temp, split=' ')
word_predict_three[i,2]<-temp[[1]][1]
word_predict_three[i,2]<-paste(word_predict_three[i,2],temp[[1]][2],sep = " ")

word_predict_three[i,3]<-temp[[1]][3]
}

names(word_predict_three)[2]<-"input"
names(word_predict_three)[3]<-"output"


#indexing
index_three_word<-data.frame("alphabets"=letters)
n=0

for(i in 1:length(word_predict_three[,1]))
{  temp<-strsplit(word_predict_three[i,2],split = '')


if(n==0 && temp[[1]][1]=='a')
{ 
  index_three_word[1,2]<-i
  n=1
}


if(n==1 && temp[[1]][1]=='b')
{ index_three_word[1,3]<-i-1
index_three_word[2,2]<-i
n=2
}


if(n==2 && temp[[1]][1]=='c')
{ index_three_word[2,3]<-i-1
index_three_word[3,2]<-i
n=3
}

if(n==3 && temp[[1]][1]=='d')
{ index_three_word[3,3]<-i-1
index_three_word[4,2]<-i
n=4
}

if(n==4 && temp[[1]][1]=='e')
{ index_three_word[4,3]<-i-1
index_three_word[5,2]<-i
n=5
}

if(n==5 && temp[[1]][1]=='f')
{ index_three_word[5,3]<-i-1
index_three_word[6,2]<-i
n=6
}


if(n==6 && temp[[1]][1]=='g')
{ index_three_word[6,3]<-i-1
index_three_word[7,2]<-i
n=7
}

if(n==7 && temp[[1]][1]=='h')
{ index_three_word[7,3]<-i-1
index_three_word[8,2]<-i
n=8
}

if(n==8 && temp[[1]][1]=='i')
{ index_three_word[8,3]<-i-1
index_three_word[9,2]<-i
n=9
}

if(n==9 && temp[[1]][1]=='j')
{ index_three_word[9,3]<-i-1
index_three_word[10,2]<-i
n=10
} 

if(n==10 && temp[[1]][1]=='k')
{ index_three_word[10,3]<-i-1
index_three_word[11,2]<-i
n=11
}

if(n==11 && temp[[1]][1]=='l')
{ index_three_word[11,3]<-i-1
index_three_word[12,2]<-i
n=12
}

if(n==12 && temp[[1]][1]=='m')
{ index_three_word[12,3]<-i-1
index_three_word[13,2]<-i
n=13
}

if(n==13 && temp[[1]][1]=='n')
{ index_three_word[13,3]<-i-1
index_three_word[14,2]<-i
n=14
}

if(n==14 && temp[[1]][1]=='o')
{ index_three_word[14,3]<-i-1
index_three_word[15,2]<-i
n=15
}

if(n==15 && temp[[1]][1]=='p')
{ index_three_word[15,3]<-i-1
index_three_word[16,2]<-i
n=16
}

if(n==16 && temp[[1]][1]=='q')
{ index_three_word[16,3]<-i-1
index_three_word[17,2]<-i
n=17
}

if(n==17 && temp[[1]][1]=='r')
{ index_three_word[17,3]<-i-1
index_three_word[18,2]<-i
n=18
}

if(n==18 && temp[[1]][1]=='s')
{ index_three_word[18,3]<-i-1
index_three_word[19,2]<-i
n=19
}

if(n==19 && temp[[1]][1]=='t')
{ index_three_word[19,3]<-i-1
index_three_word[20,2]<-i
n=20
}

if(n==20 && temp[[1]][1]=='u')
{ index_three_word[20,3]<-i-1
index_three_word[21,2]<-i
n=21
}

if(n==21 && temp[[1]][1]=='v')
{ index_three_word[21,3]<-i-1
index_three_word[22,2]<-i
n=22
}

if(n==22 && temp[[1]][1]=='w')
{ index_three_word[22,3]<-i-1
index_three_word[23,2]<-i
n=23
}

if(n==23 && temp[[1]][1]=='x')
{ index_three_word[23,3]<-i-1
index_three_word[24,2]<-i
n=24
}

if(n==24 && temp[[1]][1]=='y')
{ index_three_word[24,3]<-i-1
index_three_word[25,2]<-i
n=25
}

if(n==25 && temp[[1]][1]=='z')
{ index_three_word[25,3]<-i-1
index_three_word[26,2]<-i
n=26
}
}


index_three_word[26,3]<-length(word_predict_three[,1])





# fourgrams 
minfreq_bigram<-4

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=4,max=4, delimiters = token_delim))
bitoken
#frequency table
four_word <- data.frame(table(bitoken))
four_word
#sort according to names(alphabetical order)
sort_four <- four_word[order(four_word$bitoken,decreasing=FALSE),]
sort_four
wordcloud(sort_four$bitoken,sort_two$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Set1"),max.words=250)


#breaking words for faster access

word_predict_four<-data.frame('frequency'<-sort_four$Freq)
for(i in 1:length(sort_four$bitoken))
{ temp<-as.character(sort_four[i,1])
temp<-strsplit(temp, split=' ')
word_predict_four[i,2]<-temp[[1]][1]
word_predict_four[i,2]<-paste(word_predict_four[i,2],temp[[1]][2],sep = " ")
word_predict_four[i,2]<-paste(word_predict_four[i,2],temp[[1]][3],sep = " ")

word_predict_four[i,3]<-temp[[1]][4]
}

names(word_predict_four)[2]<-"input"
names(word_predict_four)[3]<-"output"


#indexing
index_four_word<-data.frame("alphabets"=letters)
n=0

for(i in 1:length(word_predict_four[,1]))
{  temp<-strsplit(word_predict_four[i,2],split = '')


if(n==0 && temp[[1]][1]=='a')
{ 
  index_four_word[1,2]<-i
  n=1
}


if(n==1 && temp[[1]][1]=='b')
{ index_four_word[1,3]<-i-1
index_four_word[2,2]<-i
n=2
}


if(n==2 && temp[[1]][1]=='c')
{ index_four_word[2,3]<-i-1
index_four_word[3,2]<-i
n=3
}

if(n==3 && temp[[1]][1]=='d')
{ index_four_word[3,3]<-i-1
index_four_word[4,2]<-i
n=4
}

if(n==4 && temp[[1]][1]=='e')
{ index_four_word[4,3]<-i-1
index_four_word[5,2]<-i
n=5
}

if(n==5 && temp[[1]][1]=='f')
{ index_four_word[5,3]<-i-1
index_four_word[6,2]<-i
n=6
}


if(n==6 && temp[[1]][1]=='g')
{ index_four_word[6,3]<-i-1
index_four_word[7,2]<-i
n=7
}

if(n==7 && temp[[1]][1]=='h')
{ index_four_word[7,3]<-i-1
index_four_word[8,2]<-i
n=8
}

if(n==8 && temp[[1]][1]=='i')
{ index_four_word[8,3]<-i-1
index_four_word[9,2]<-i
n=9
}

if(n==9 && temp[[1]][1]=='j')
{ index_four_word[9,3]<-i-1
index_four_word[10,2]<-i
n=10
} 

if(n==10 && temp[[1]][1]=='k')
{ index_four_word[10,3]<-i-1
index_four_word[11,2]<-i
n=11
}

if(n==11 && temp[[1]][1]=='l')
{ index_four_word[11,3]<-i-1
index_four_word[12,2]<-i
n=12
}

if(n==12 && temp[[1]][1]=='m')
{ index_four_word[12,3]<-i-1
index_four_word[13,2]<-i
n=13
}

if(n==13 && temp[[1]][1]=='n')
{ index_four_word[13,3]<-i-1
index_four_word[14,2]<-i
n=14
}

if(n==14 && temp[[1]][1]=='o')
{ index_four_word[14,3]<-i-1
index_four_word[15,2]<-i
n=15
}

if(n==15 && temp[[1]][1]=='p')
{ index_four_word[15,3]<-i-1
index_four_word[16,2]<-i
n=16
}

if(n==16 && temp[[1]][1]=='q')
{ index_four_word[16,3]<-i-1
index_four_word[17,2]<-i
n=17
}

if(n==17 && temp[[1]][1]=='r')
{ index_four_word[17,3]<-i-1
index_four_word[18,2]<-i
n=18
}

if(n==18 && temp[[1]][1]=='s')
{ index_four_word[18,3]<-i-1
index_four_word[19,2]<-i
n=19
}

if(n==19 && temp[[1]][1]=='t')
{ index_four_word[19,3]<-i-1
index_four_word[20,2]<-i
n=20
}

if(n==20 && temp[[1]][1]=='u')
{ index_four_word[20,3]<-i-1
index_four_word[21,2]<-i
n=21
}

if(n==21 && temp[[1]][1]=='v')
{ index_four_word[21,3]<-i-1
index_four_word[22,2]<-i
n=22
}

if(n==22 && temp[[1]][1]=='w')
{ index_four_word[22,3]<-i-1
index_four_word[23,2]<-i
n=23
}

if(n==23 && temp[[1]][1]=='x')
{ index_four_word[23,3]<-i-1
index_four_word[24,2]<-i
n=24
}

if(n==24 && temp[[1]][1]=='y')
{ index_four_word[24,3]<-i-1
index_four_word[25,2]<-i
n=25
}

if(n==25 && temp[[1]][1]=='z')
{ index_four_word[25,3]<-i-1
index_four_word[26,2]<-i
n=26
}
}


index_four_word[26,3]<-length(word_predict_four[,1])





# fivegrams 
minfreq_bigram<-5

token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=5,max=5, delimiters = token_delim))
bitoken
#frequency table
five_word <- data.frame(table(bitoken))
five_word
#sort according to names(alphabetical order)
sort_five <- five_word[order(five_word$bitoken,decreasing=FALSE),]
sort_five
wordcloud(sort_five$bitoken,sort_five$Freq,random.order=FALSE,scale = c(2,0.35),min.freq = minfreq_bigram,colors = brewer.pal(8,"Set1"),max.words=250)


#breaking words for faster access

word_predict_five<-data.frame('frequency'<-sort_five$Freq)
for(i in 1:length(sort_five$bitoken))
{ temp<-as.character(sort_five[i,1])
temp<-strsplit(temp, split=' ')
word_predict_five[i,2]<-temp[[1]][1]
word_predict_five[i,2]<-paste(word_predict_five[i,2],temp[[1]][2],sep = " ")
word_predict_five[i,2]<-paste(word_predict_five[i,2],temp[[1]][3],sep = " ")
word_predict_five[i,2]<-paste(word_predict_five[i,2],temp[[1]][4],sep = " ")

word_predict_five[i,3]<-temp[[1]][5]
}

names(word_predict_five)[2]<-"input"
names(word_predict_five)[3]<-"output"


#indexing
index_five_word<-data.frame("alphabets"=letters)
n=0

for(i in 1:length(word_predict_five[,1]))
{  temp<-strsplit(word_predict_five[i,2],split = '')


if(n==0 && temp[[1]][1]=='a')
{ 
  index_five_word[1,2]<-i
  n=1
}


if(n==1 && temp[[1]][1]=='b')
{ index_five_word[1,3]<-i-1
index_five_word[2,2]<-i
n=2
}


if(n==2 && temp[[1]][1]=='c')
{ index_five_word[2,3]<-i-1
index_five_word[3,2]<-i
n=3
}

if(n==3 && temp[[1]][1]=='d')
{ index_five_word[3,3]<-i-1
index_five_word[4,2]<-i
n=4
}

if(n==4 && temp[[1]][1]=='e')
{ index_five_word[4,3]<-i-1
index_five_word[5,2]<-i
n=5
}

if(n==5 && temp[[1]][1]=='f')
{ index_five_word[5,3]<-i-1
index_five_word[6,2]<-i
n=6
}


if(n==6 && temp[[1]][1]=='g')
{ index_five_word[6,3]<-i-1
index_five_word[7,2]<-i
n=7
}

if(n==7 && temp[[1]][1]=='h')
{ index_five_word[7,3]<-i-1
index_five_word[8,2]<-i
n=8
}

if(n==8 && temp[[1]][1]=='i')
{ index_five_word[8,3]<-i-1
index_five_word[9,2]<-i
n=9
}

if(n==9 && temp[[1]][1]=='j')
{ index_five_word[9,3]<-i-1
index_five_word[10,2]<-i
n=10
} 

if(n==10 && temp[[1]][1]=='k')
{ index_five_word[10,3]<-i-1
index_five_word[11,2]<-i
n=11
}

if(n==11 && temp[[1]][1]=='l')
{ index_five_word[11,3]<-i-1
index_five_word[12,2]<-i
n=12
}

if(n==12 && temp[[1]][1]=='m')
{ index_five_word[12,3]<-i-1
index_five_word[13,2]<-i
n=13
}

if(n==13 && temp[[1]][1]=='n')
{ index_five_word[13,3]<-i-1
index_five_word[14,2]<-i
n=14
}

if(n==14 && temp[[1]][1]=='o')
{ index_five_word[14,3]<-i-1
index_five_word[15,2]<-i
n=15
}

if(n==15 && temp[[1]][1]=='p')
{ index_five_word[15,3]<-i-1
index_five_word[16,2]<-i
n=16
}

if(n==16 && temp[[1]][1]=='q')
{ index_five_word[16,3]<-i-1
index_five_word[17,2]<-i
n=17
}

if(n==17 && temp[[1]][1]=='r')
{ index_five_word[17,3]<-i-1
index_five_word[18,2]<-i
n=18
}

if(n==18 && temp[[1]][1]=='s')
{ index_five_word[18,3]<-i-1
index_five_word[19,2]<-i
n=19
}

if(n==19 && temp[[1]][1]=='t')
{ index_five_word[19,3]<-i-1
index_five_word[20,2]<-i
n=20
}

if(n==20 && temp[[1]][1]=='u')
{ index_five_word[20,3]<-i-1
index_five_word[21,2]<-i
n=21
}

if(n==21 && temp[[1]][1]=='v')
{ index_five_word[21,3]<-i-1
index_five_word[22,2]<-i
n=22
}

if(n==22 && temp[[1]][1]=='w')
{ index_five_word[22,3]<-i-1
index_five_word[23,2]<-i
n=23
}

if(n==23 && temp[[1]][1]=='x')
{ index_five_word[23,3]<-i-1
index_five_word[24,2]<-i
n=24
}

if(n==24 && temp[[1]][1]=='y')
{ index_five_word[24,3]<-i-1
index_five_word[25,2]<-i
n=25
}

if(n==25 && temp[[1]][1]=='z')
{ index_five_word[25,3]<-i-1
index_five_word[26,2]<-i
n=26
}
}


index_five_word[26,3]<-length(word_predict_five[,1])




#onegram



token_delim <- " \\t\\r\\n.!?,;\"()"
bitoken <- NGramTokenizer(mycorpus, Weka_control(min=1,max=1, delimiters = token_delim))
bitoken
#frequency table
one_word <- data.frame(table(bitoken))
one_word
j<<-1
sort_one<-data.frame()
for(i in 1:length(one_word[,1]))
{
  temp<-strsplit(as.character(one_word[i,1]),split = "")
  if(length(temp[[1]])==3)
  {sort_one[j,1]<-one_word[i,1]
  sort_one[j,2]<-one_word[i,2]
  j<<-j+1
  }}
names(sort_one)[1]<-"word"
names(sort_one)[2]<-"Freq"
#sort according to names(alphabetical order)
sort_one <- sort_one[order(sort_one$Freq),]
sort_one
index_a<-(length(sort_one[,1])-9)
three_letter_words<-as.vector(sort_one[index_a:length(sort_one[,1]),1])  
three_letter_words








flag<-0
starting_index_searchspace<-0
ending_index_searchspace<-0


search_word<-function(word_table,start,end,para)
{ highest<-0
  output<-c()
  
  for(i in start:end)
  { 
    if(strcmp(para,word_table[i,2]) && as.integer(word_table[i,1])>highest)
    { 
      
      highest<-as.integer(word_table[i,1])
       output<-word_table[i,3]
      
      
} }
    
    if(highest==0)
    {
      return(0)
    }
    else return(output)
  
  
}


index_finder<-function(para,index_table)
{
  index<-strsplit(para,split = "")
  for(i in 1:length(index_table[,1]))
  {
    if(index[[1]][1]==index_table[i,1])
    {
      starting_index_searchspace<<-index_table[i,2]
      ending_index_searchspace<<-index_table[i,3]
      
      break
    }
  }
  
  
}



predict_nextword<-function(para)
{ 
  index_finder(para,index_five_word)
  
  flag<-search_word(word_predict_five,starting_index_searchspace,ending_index_searchspace,para)
  
  if(flag==0)
  { temp<-strsplit(para,split = " ")
  
    temp[[1]][1]<-paste(temp[[1]][1]," ")
    para<-temp[[1]][2]
    for(k in 3:length(temp[[1]]))
    { para<-paste(para,temp[[1]][k],sep = " ")
      
    }
    index_finder(para,index_four_word)
    
    flag<-search_word(word_predict_four,starting_index_searchspace,ending_index_searchspace,para)
     
    if(flag==0)
    { temp<-strsplit(para,split = " ")
    para<-temp[[1]][2]
    for(k in 3:length(temp[[1]]))
    { para<-paste(para,temp[[1]][k],sep = " ")
    
    }
    index_finder(para,index_three_word)
    flag<-search_word(word_predict_three,starting_index_searchspace,ending_index_searchspace,para)
    
    if(flag==0)
    { temp<-strsplit(para,split = " ")
    para<-temp[[1]][2]
    
    index_finder(para,index_two_word)
    flag<-search_word(word_predict_two,starting_index_searchspace,ending_index_searchspace,para)
    
    if(flag==0)
    {
      acc_to_first_character(para)
      print(random_three_letter_predict(three_letter_words))
    }
    else
    {
      print(flag)
    }
    
    
    }
    else
    {
      print(flag)
    }
    
    }
    
    else
    {
      print(flag)
    }
  }
  else
  {
    print(flag)
  }
  
}

input<-read.csv("input.csv",sep = "\t")
para<-input[1,1]
para<-as.character(para)
words<-c()
temp<-strsplit(para,split = " ")
a= length(temp[[1]])-3
words<-temp[[1]][a]
for(i in (a+1):length(temp[[1]]))
{ words<-paste(words,temp[[1]][i])
  
}
words

words<-tolower(words)

words

predict_nextword(words)










acc_to_first_character<-function(para)
{
  index_finder(para,index_two_word)
  charac_word_database<-word_predict_two[starting_index_searchspace:ending_index_searchspace,]
  names(charac_word_database)[1]<-"freq"
   charac_word_database<-charac_word_database[order(charac_word_database$freq),]
  
   print(charac_word_database[length(charac_word_database[,1]),3])
  
}





random_three_letter_predict<-function(three_letter_words)
{  ran<-sample(1:10, 1, replace=TRUE)
  return(three_letter_words[ran])
}

