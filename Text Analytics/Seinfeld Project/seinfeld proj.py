# -*- coding: utf-8 -*-
"""
Created on Sun Oct 15 18:11:17 2023

@author: thfar
"""

# *****************I did not remove any punctuation or do any regex

import pandas as pd
import matplotlib.pyplot as plt
import spacy
import nltk
from collections import Counter
from collections import OrderedDict
import numpy as np
import re
import gensim
import string
from sentiment_module import sentiment
###########################################################################
####################### Reading in the data, EDA #################################
################################################################################
episodes = pd.read_csv("C:/Users/thfar/OneDrive/Documents/IAA/IAA - AA502/Text Analytics/Seinfeld Project/episode_info.csv")
scripts = pd.read_csv("C:/Users/thfar/OneDrive/Documents/IAA/IAA - AA502/Text Analytics/Seinfeld Project/scripts.csv")

scripts.info()

#Looks at missing values. Dialogu column has 10 missing values, so we remove them
scripts.isnull().sum()
scripts = scripts.dropna(axis =0) #axis = 0 removes rows, = 1 removes columns

#count of character counts, many characters have "Elaine (laughs at Toby)"
#removing everything after the parenthees
scripts['Character'].value_counts()

scripts['Character'] = scripts['Character'].str.split('(').str[0] #gets character name without parentheses
scripts['Character'] = scripts['Character'].str.strip() #removes any extra spaces

#checking to make sure "ELAINE" is counted for by itself with no other ()
scripts[scripts['Character'].str.contains('ELAINE')]['Character'].value_counts()

###################################################
################# Plotting data, summary stats  #############
#################################################################

#top 5 characters speaking in show
scripts['Character'].value_counts()[:5].plot(kind='barh')

#number of episodes by season
episodes.groupby(['Season'])['EpisodeNo'].count().plot(kind = 'bar')

#top speakers by season
def plot_lines(season = None, episode = None, top_n = 6, ax = None):
    filtered_scripts = scripts
    if season:
        filtered_scripts = filtered_scripts[filtered_scripts['Season'] == season]
    if episode:
        filtered_scripts = filtered_scripts[filtered_scripts['SEID'] == episode]
    filtered_scripts['Character'].value_counts().head(top_n).plot(kind = 'bar', ax = ax)

fig, axes = plt.subplots(ncols=3, nrows=3, figsize=(10, 20), dpi=100)
for i in range(9):
    season = i + 1
    row = i//3
    col = i%3
    plot_lines(season = season, ax = axes[row][col])
    axes[row][col].set_title(f'Season {season}', fontsize=12)
plt.show()


#subsetting scripts on people and seasons
jerry_scripts = scripts[scripts['Character'] == "JERRY"].reset_index()
elaine_scripts = scripts[scripts['Character'] == "ELAINE"].reset_index()
george_scripts =scripts[scripts['Character'] == "GEORGE"].reset_index()
kramer_scripts = scripts[scripts['Character'] == "KRAMER"].reset_index()
newman_scripts = scripts[scripts['Character'] == "NEWMAN"].reset_index()

ppl_scr = [jerry_scripts, elaine_scripts, george_scripts, kramer_scripts, newman_scripts]

season1_scripts = scripts[scripts['Season'] == 1].reset_index()
season2_scripts = scripts[scripts['Season'] == 2].reset_index()
season3_scripts = scripts[scripts['Season'] == 3].reset_index()
season4_scripts = scripts[scripts['Season'] == 4].reset_index()
season5_scripts = scripts[scripts['Season'] == 5].reset_index()
season6_scripts = scripts[scripts['Season'] == 6].reset_index()
season7_scripts = scripts[scripts['Season'] == 7].reset_index()
season8_scripts = scripts[scripts['Season'] == 8].reset_index()
season9_scripts = scripts[scripts['Season'] == 9].reset_index()

szn_scr = [season1_scripts, season2_scripts, season3_scripts, season4_scripts, 
           season5_scripts, season6_scripts, season7_scripts, season8_scripts, 
           season9_scripts]

####################################################################################
###########################  Word Frequency #######################################
###################################################################################
nlp = spacy.load( 'en_core_web_lg' )
############################################################
######### Term Frequency Representation #####################
###############################################################

#By Character
for j in ppl_scr:
    d = {}
    print("Character: ", j['Character'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        for token in t_nlp:
            term = token.text
            d[term] = (1 if term not in d else d[ term ] + 1 )
        
    sorted_dict = sorted(d.items(), key = lambda x:x[1], reverse = True) #lists most popular words in descending order
    converted_dict = dict(sorted_dict)
    print(list(converted_dict.items())[:50]) #list top 50
    
#By Seasons
for j in szn_scr:
    d = {}
    print("Season ", j['Season'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        for token in t_nlp:
            term = token.text
            d[term] = (1 if term not in d else d[ term ] + 1 )
        
    sorted_dict = sorted(d.items(), key = lambda x:x[1], reverse = True)
    converted_dict = dict(sorted_dict)
    print(list(converted_dict.items())[:50])


############################################################
######### Bigram Frequency Representation ##################
###############################################################

#By Character
for j in ppl_scr:
    d = {}
    print("Character: ", j['Character'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        for pp in range( 1, len( t_nlp ) ):
            bigram = (t_nlp[ pp - 1 ].text,t_nlp[ pp ].text )
            d[ bigram ] = ( 1 if bigram not in d else d[ bigram ] + 1 )
        
    sorted_dict = sorted(d.items(), key = lambda x:x[1], reverse = True) #lists most popular words in descending order
    converted_dict = dict(sorted_dict)
    print(list(converted_dict.items())[:50]) #list top 50


#By Seasons
for j in szn_scr:
    d = {}
    print("Season ", j['Season'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        for pp in range( 1, len( t_nlp ) ):
            bigram = (t_nlp[ pp - 1 ].text,t_nlp[ pp ].text )
            d[ bigram ] = ( 1 if bigram not in d else d[ bigram ] + 1 )
        
    sorted_dict = sorted(d.items(), key = lambda x:x[1], reverse = True) #lists most popular words in descending order
    converted_dict = dict(sorted_dict)
    print(list(converted_dict.items())[:50]) #list top 50


###############################################################
######## Parts of speech Frequency Representation #############
##################################################################

#By Character
for j in ppl_scr:
    x = []
    print("Character: ", j['Character'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        POS = [ (token.pos_) for token in t_nlp ]
        x.append(POS)
        
    flat_list = [item for sublist in x for item in sublist] #flattens list so we can get value counts of each POS
    flat_list = pd.DataFrame(flat_list)
        
    print(flat_list.value_counts()) #shows value counts
    flat_list.value_counts()[:8].plot(kind='barh')
    plt.title(j['Character'][0])
    plt.show()

#By Seasons
for j in szn_scr:
    x = []
    print("Season ", j['Season'][0])
    for i in range(len(j)):
        t_nlp = nlp( j['Dialogue'][i] ) 
        POS = [ (token.pos_) for token in t_nlp ]
        x.append(POS)
        
    flat_list = [item for sublist in x for item in sublist]
    flat_list = pd.DataFrame(flat_list)
        
    print(flat_list.value_counts())
    flat_list.value_counts()[:8].plot(kind='barh')
    plt.title(j['Season'][0])
    plt.show()


#########################################################
##################### Word Cloud #######################
##########################################################





###################################################################################
######################## Term Vectors #############################################
###################################################################################

#####################################################
################ Stop Word Removal + Porter Stemming #####
#######################################################
stop_words = nltk.corpus.stopwords.words( 'english' )
porter = nltk.stem.porter.PorterStemmer()

punc = re.compile( '[%s]' % re.escape( string.punctuation ) )

   
#By Character
for j in ppl_scr:
    term_vec = [ ]
    print("Character: ", j['Character'][0])
    for d in j['Dialogue']:
        d = d.lower()
        d = punc.sub( '', d) #removes punctuation
        term_vec.append( nltk.word_tokenize(d))  #tokenizes docs
    
    for i in range(0, len(term_vec)):
        term_list = [ ]
        
        for term in term_vec[ i ]: #removes stop words
            if term not in stop_words:
                term_list.append( term )
        term_vec[i] = term_list
        
    for q in range( 0, len( term_vec ) ): #porter stemming
        for ww in range( 0, len( term_vec[ q ] ) ):
            term_vec[ q ][ ww ] = porter.stem( term_vec[ q ][ ww ] )
        
    flat_list = [item for sublist in term_vec for item in sublist] #flattens list so we can get value counts of each POS
    flat_list = pd.DataFrame(flat_list)
        
    print(flat_list.value_counts()) #shows value counts


#By Season
for j in szn_scr:
    term_vec = [ ]
    print("Season ", j['Season'][0])
    for d in j['Dialogue']:
        d = d.lower()
        d = punc.sub( '', d) #removes punctuation
        term_vec.append( nltk.word_tokenize(d))  #tokenizes docs
    
    for i in range(0, len(term_vec)):
        term_list = [ ]
        
        for term in term_vec[ i ]: #stop words
            if term not in stop_words:
                term_list.append( term )
        term_vec[i] = term_list
        
    for q in range( 0, len( term_vec ) ): #porter stemming
        for ww in range( 0, len( term_vec[ q ] ) ):
            term_vec[ q ][ ww ] = porter.stem( term_vec[ q ][ ww ] )
            
    flat_list = [item for sublist in term_vec for item in sublist] #flattens list so we can get value counts of each POS
    flat_list = pd.DataFrame(flat_list)
        
    print(flat_list.value_counts()) #shows value counts




###################################################################################
######################## Similarity #############################################
###################################################################################

#####################################################
################ TF-IDF ############################
#######################################################

pass



#####################################################
################ LDA  ############################
#######################################################

pass



#####################################################
################ Word Embeddings ############################
#######################################################

pass



###################################################################################
######################## Sentiment Analysis #############################################
###################################################################################

#####################################################
################ Term Sentiment ############################
#######################################################

#By Character
for j in ppl_scr:
    term_vec = [ ]
    feelings = [ ]
    feel_vals = [ ]
    print("Character: ", j['Character'][0], "- Top 5 + Bottom 5 Feelings")
    for d in j['Dialogue']:
        d = d.lower()
        d = punc.sub( '', d) #removes punctuation
        term_vec.append( nltk.word_tokenize(d))  #tokenizes docs
        
       
    for zz in range(len(term_vec)):
        #print(sentiment.sentiment(term_vec[zz]))
        #print(sentiment.describe(term_vec[zz]))
        feelings.append(sentiment.describe(term_vec[zz]))
        feel_vals.append(sentiment.sentiment(term_vec[zz]))
    feelings = pd.DataFrame(feelings)
    print(feelings.value_counts().head())
    print(feelings.value_counts().tail())
    
    feel_vals = pd.DataFrame(feel_vals)
    plt.scatter(feel_vals['valence'], feel_vals['arousal'])
    plt.title(j['Character'][0])
    plt.show()


#By Season
for j in szn_scr:
    term_vec = [ ]
    feelings = [ ]
    feel_vals = []
    print("Season ", j['Season'][0], "- Top 5 + Bottom 5 Feelings")
    for d in j['Dialogue']:
        d = d.lower()
        d = punc.sub( '', d) #removes punctuation
        term_vec.append( nltk.word_tokenize(d))  #tokenizes docs
        
    for zz in range(len(term_vec)):
        #print(sentiment.sentiment(term_vec[zz]))
        #print(sentiment.describe(term_vec[zz]))
        feelings.append(sentiment.describe(term_vec[zz]))
        feel_vals.append(sentiment.sentiment(term_vec[zz]))
    feelings = pd.DataFrame(feelings)
    print(feelings.value_counts().head())
    print(feelings.value_counts().tail())
    
    feel_vals = pd.DataFrame(feel_vals)
    plt.scatter(feel_vals['valence'], feel_vals['arousal'])
    plt.title(j['Season'][0])
    plt.show()

