# install_github("trinker/qdapRegex")
# install.packages("lsa")
# install.packages("SnowballC")
# install.packages("tm")
# install.packages("openNLP")
library(lsa)
library(openNLP)
library(NLP)
library (devtools)
library(qdapRegex)
library(stringr)
library(tm)

# options(java.parameters = "-Xmx5g")

gc()
rm(list=ls())

# POS tagging


# Collecting Words
char_vector <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/Data Set/facet_morality.txt")
# length(char_vector)
data.subset <- sample(char_vector,10000)
rm(char_vector)
gc()

library('lsa')
library(openNLP)
library(NLP)
library (devtools)
library(qdapRegex)
library(stringr)
library(tm)

##### Data Preprocessesing #####
# removing urls
data.subset <- rm_url(data.subset, pattern=pastex("@rm_twitter_url", "@rm_url"))
# removing special characters and numbers
data.subset <- gsub("[^A-Za-z///' ]", "", data.subset)
# removing stopwords
data.subset <- removeWords(data.subset,stopwords('en'))
# removing badwords
badwords<-readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
badwords<-badwords[-(which(badwords%in%c("refugee","reject","remains","screw",
                                         "welfare", "sweetness","shoot","sick","shooting","servant",
                                         "radical","racial","racist","republican","public","molestation","mexican",
                                         "looser","lesbian","liberal","kill","killing","killer","heroin","fraud",
                                         "fire","fight","fairy","^die","death","desire","deposit","crash","^crim",
                                         "crack","^color","cigarette","church","^christ","cancer",
                                         "^catholic","cemetery","buried","burn","^bomb","^beast","attack",
                                         "australian","balls","baptist","^addict","abuse","abortion","amateur","asian",
                                         "aroused","angry","arab","bible")==TRUE))]
data.subset <- removeWords(data.subset,badwords)
# removing two letters and one letter words
data.subset <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", data.subset)




##### POS Tagging #####
tagPOS <-  function(x, ...) {
  str <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  antn <- Annotation(1L, "sentence", 1L, nchar(str))
  antn <- annotate(str, word_token_annotator, antn)
  antnPOSTag <- annotate(str, Maxent_POS_Tag_Annotator(), antn)
  antnPOSTagWord <- antnPOSTag[antnPOSTag$type == "word"]
  tgPOS <- unlist(lapply(antnPOSTagWord$features, `[[`, "POS"))
  tgdPOS <- paste(sprintf("%s/%s", str[antnPOSTagWord], tgPOS), collapse = " ")
  list(tgdPOS = tgdPOS, tgPOS = tgPOS)
}

tagged_str <-  tagPOS(data.subset)
# tagged_str[[1]]

# Fetching adjectives
adjOpnnss <- unlist(str_extract_all(unlist(tagged_str[[1]]), "\\w+(?=\\/JJ)"))
adjOpnnss <- tolower(adjOpnnss)
adjOpnnss <- unique(names(sort(table(adjOpnnss), decreasing=T)[1:(0.1*length(adjOpnnss))]))


write.table(adjOpnnss, "F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_morality.txt", sep="\t")




##############################################################################################################################



### Validation ###

#### Reading Traits#####
char_achievement_striving <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_achievement_striving.txt")
char_activity_level <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_activity_level.txt")
char_adventurousness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_adventurousness.txt")
char_altruism <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_altruism.txt")
char_anger <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_anger.txt")
char_anxiety <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_anxiety.txt")
char_artistic_interests <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_artistic_interests.txt")
char_assertiveness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_assertiveness.txt")
char_agreeableness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_big5_agreeableness.txt")
char_conscientiousness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_big5_conscientiousness.txt")
char_extraversion <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_big5_extraversion.txt")
char_neuroticism <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_big5_neuroticism.txt")
char_cautiousness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_cautiousness.txt")
char_challenge <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_challenge.txt")
char_cheerfulness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_cheerfulness.txt")
char_closeness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_closeness.txt")
char_conservation <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_conservation.txt")
char_cooperation <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_cooperation.txt")
char_curiosity <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_curiosity.txt")
char_depression <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_depression.txt")
char_dutifulness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_dutifulness.txt")
char_emotionality <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_emotionality.txt")
char_excitement <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_excitement.txt")
char_excitement_seeking <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_excitement_seeking.txt")
char_friendliness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_friendliness.txt")
char_gregariousness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_gregariousness.txt")
char_harmony <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_harmony.txt")
char_hedonism <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_hedonism.txt")
char_ideal <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_ideal.txt")
char_imagination <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_imagination.txt")
char_immoderation <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_immoderation.txt")
char_intellect <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_intellect.txt")
char_liberalism <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_liberalism.txt")
char_liberty <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_liberty.txt")
char_love <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_love.txt")
char_modesty <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_modesty.txt")
char_morality <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_morality.txt")
char_openness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_big5_openness.txt")
char_openness_to_change <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_openness_to_change.txt")
char_orderliness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_orderliness.txt")
char_practicality <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_practicality.txt")
char_self_consciousness <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_self_consciousness.txt")
char_self_discipline <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_self_discipline.txt")
char_self_efficacy <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_self_efficacy.txt")
char_self_enhancement <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_self_enhancement.txt")
char_self_expression <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_self_expression.txt")
char_stability <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_stability.txt")
char_structure <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_structure.txt")
char_sympathy <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_sympathy.txt")
char_transcendence <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_transcendence.txt")
char_trust <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_trust.txt")
char_vulnerability <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/adj_vulnerability.txt")




##### Archetypes #####
# rm(td)
td = tempfile()
dir.create(td)

#### Innocent #####
write( unique(c(char_dutifulness,char_friendliness,char_morality,char_openness,char_sympathy,char_trust,
                char_ideal,char_transcendence)), file=paste(td, "D1", sep="/t"))

#### Hero #####
write( unique(c(char_achievement_striving,char_activity_level,char_assertiveness,char_excitement_seeking,
                char_self_discipline,char_challenge,char_self_enhancement)), file=paste(td, "D2", sep="/t"))

#### Regular Guy #####
write( unique(c(char_cheerfulness,char_cooperation,char_friendliness,char_modesty,char_trust,
                char_self_expression,char_hedonism)), file=paste(td, "D3", sep="/t"))

#### Nurturer #####
write( unique(c(char_altruism,char_cheerfulness,char_dutifulness,char_sympathy,char_trust,char_love,
                char_transcendence)), file=paste(td, "D4", sep="/t"))

#### Creator #####
write( unique(c(char_artistic_interests,char_emotionality,char_imagination,char_immoderation,char_vulnerability,
                char_self_expression,char_self_enhancement)), file=paste(td, "D5", sep="/t"))

#### Explorer #####
write( unique(c(char_activity_level,char_adventurousness,char_assertiveness,char_excitement_seeking,
                char_immoderation,char_excitement,char_hedonism)), file=paste(td, "D6", sep="/t"))

#### Rebel #####
write( unique(c(char_anger,char_adventurousness,char_liberalism,char_excitement_seeking,
                char_immoderation,char_liberty,char_hedonism)), file=paste(td, "D7", sep="/t"))

#### Lover #####
write( unique(c(char_cooperation,char_emotionality,char_friendliness,char_self_consciousness,char_sympathy,
                char_love,char_self_enhancement)), file=paste(td, "D8", sep="/t"))

#### Magician #####
write( unique(c(char_achievement_striving,char_activity_level,char_adventurousness,char_imagination,
                char_intellect,char_curiosity,char_openness_to_change)), file=paste(td, "D9", sep="/t"))

#### Ruler #####
write( unique(c(char_anger,char_assertiveness,char_morality,char_orderliness,
                char_self_discipline,char_structure,char_conservation)), file=paste(td, "D10", sep="/t"))

#### Jester #####
write( unique(c(char_altruism,char_artistic_interests,char_friendliness,char_gregariousness,char_immoderation,
                char_harmony,char_transcendence)), file=paste(td, "D11", sep="/t"))

#### Sage #####
write( unique(c(char_achievement_striving,char_assertiveness,char_morality,char_dutifulness,
                char_self_discipline,char_structure,char_conservation)), file=paste(td, "D12", sep="/t"))

##### Tokenizing text input #####
char_input <- readLines("F:/UCONN/Spring/Big Data Strategic Mktg/Project/TextFile/input.txt")

gc()

library('lsa')
library(openNLP)
library(NLP)
library (devtools)
library(qdapRegex)
library(stringr)
library(tm)
#removing urls
data.subset <- rm_url(char_input, pattern=pastex("@rm_twitter_url", "@rm_url"))
#removing special characters and numbers
data.subset <- gsub("[^A-Za-z///' ]", "", data.subset)
#removing stopwords
data.subset <- removeWords(data.subset,stopwords('en'))
#removing badwords
badwords<-readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt")
badwords<-badwords[-(which(badwords%in%c("refugee","reject","remains","screw",
                                         "welfare", "sweetness","shoot","sick","shooting","servant",
                                         "radical","racial","racist","republican","public","molestation","mexican",
                                         "looser","lesbian","liberal","kill","killing","killer","heroin","fraud",
                                         "fire","fight","fairy","^die","death","desire","deposit","crash","^crim",
                                         "crack","^color","cigarette","church","^christ","cancer",
                                         "^catholic","cemetery","buried","burn","^bomb","^beast","attack",
                                         "australian","balls","baptist","^addict","abuse","abortion","amateur","asian",
                                         "aroused","angry","arab","bible")==TRUE))]
data.subset <- removeWords(data.subset,badwords)
#removing two letters and one letter word
data.subset <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", data.subset)

##### POS Tagging #####
tagPOS <-  function(x, ...) {
  str <- as.String(x)
  word_token_annotator <- Maxent_Word_Token_Annotator()
  antn <- Annotation(1L, "sentence", 1L, nchar(str))
  antn <- annotate(str, word_token_annotator, antn)
  antnPOSTag <- annotate(str, Maxent_POS_Tag_Annotator(), antn)
  antnPOSTagWord <- antnPOSTag[antnPOSTag$type == "word"]
  tgPOS <- unlist(lapply(antnPOSTagWord$features, `[[`, "POS"))
  tgdPOS <- paste(sprintf("%s/%s", str[antnPOSTagWord], tgPOS), collapse = " ")
  list(tgdPOS = tgdPOS, tgPOS = tgPOS)
}

# Checking POS
tagged_str <-  tagPOS(data.subset)
# tagged_str[[1]]

# Fetching adjectives
adjInput <- unlist(str_extract_all(unlist(tagged_str[[1]]), "\\w+(?=\\/JJ)"))
adjInput <- tolower(adjInput)
adjInput <- unique(names(sort(table(adjInput), decreasing=T)))

write(adjInput, file=paste(td, "Inpt", sep="/t"))
 
##### read files into a document-term matrix #####
myMatrix = textmatrix(td, minWordLength=1)

##### Calculate cosine similarity #####
resMat <- array()
for (i in 1:12)
{
  resMat[i] <- lsa::cosine(myMatrix[,i], myMatrix[,13])
}


names(resMat) <- c("Innocent","Hero","Regular Guy","Nurturer","Creator","Explorer","Rebel",
                   "Lover","Magician","Ruler","Jester","Sage")
resMat

# clean up
unlink(td, recursive=TRUE)
