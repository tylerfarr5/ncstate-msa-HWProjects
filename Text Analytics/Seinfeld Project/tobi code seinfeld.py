# -*- coding: utf-8 -*-
"""
Created on Mon Oct 16 10:48:40 2023

@author: thfar
"""

---
title: "Seinfeld"
format: html
---

# importing libraries
# to activate library in terminal run -> source SeinfeldEnv/bin/activate
```{python}
import pandas as pd
import spacy
import nltk
import matplotlib.pyplot as plt
import numpy as np
import re
from collections import Counter
import gensim
import string
from nltk.sentiment.vader import SentimentIntensityAnalyzer
nltk.download('averaged_perceptron_tagger')
nltk.download('punkt')
nltk.download('stopwords')
nltk.download( 'vader_lexicon' )
from pyvis.network import Network
from PIL import Image
import matplotlib.patches as mpatches


```

# cmd shift I to add a new cell
# Reading the data 
```{python}
scripts = pd.read_csv('scripts.csv')
epi = pd.read_csv('episode_info.csv')
```

# cleaning up the main characters
```{python}
pattern = r'(.+?)(?:\s*\((.*?)\))?$'  # Capture main name and optional content in parentheses

# Use str.extract to apply the regular expression pattern to the "Names" column.
# This will capture both the main name and the contents of parentheses.
matches = scripts['Character'].str.extract(pattern) #extracting anything that is inside parenthesis
matches.columns = ['Main_Name', 'Parenthesis_Content']
matches['Parenthesis_Content'].fillna('', inplace=True)

scripts_cleaner = pd.concat([scripts, matches], axis=1)
scripts_cleaner['Main_Name'] = scripts_cleaner['Main_Name'].str.lower()

#selecting the characters we want to analyze
scripts_cleaner['Main_Name'].value_counts().head(50)

scripts_cleaner['Dialogue'] = scripts_cleaner['Parenthesis_Content'] + ' ' + scripts_cleaner['Dialogue']

characters = ['jerry', 'george', 'kramer', 'elaine', 'newman', 'peterman', 'puddy', 'tim', 'frank', 'mickey', 'bania']

#subsetting character names to the main characters only
main_characters = scripts_cleaner[scripts_cleaner['Main_Name'].isin(characters)].reset_index()

#now we have a dataframe with the main character, their dialogue, and if they have anything in parenthesis
```

# creating a corpus of text for each character

```{python}
grouped = main_characters.groupby(['Main_Name', 'Season'])

# looking at certain characters by season
Grouped_characters = grouped['Dialogue'].agg(lambda x: ' '.join(x)).reset_index()

# looking at certain characters all together
grouped2 = main_characters.groupby(['Main_Name'])
Grouped_characters_totals = grouped2['Dialogue'].agg(lambda x: ' '.join(x)).reset_index()
```

# creating term vectors for each character for each season
# can easily switch between Grouped_characters_totals and Grouped_characters
```{python}
# Remove punctuation, then tokenize documents
punc = re.compile( '[%s]' % re.escape( string.punctuation ) )
term_vec = []

Grouped_characters = Grouped_characters_totals.copy()
original_order = Grouped_characters['Main_Name'].unique()

for d in Grouped_characters['Dialogue']:
    d = d.lower()
    d = punc.sub('', d)
    term_vec.append(nltk.word_tokenize(d))

# Print resulting term vectors

for vec in term_vec:
    print(vec)  
    
raw_terms = term_vec[:]

```

# creating stop terms 

```{python}
stop_words = nltk.corpus.stopwords.words( 'english' )

for i in range( 0, len( term_vec ) ):
    term_list = [ ]

    for term in term_vec[ i ]:
        if term not in stop_words:
            term_list.append( term )

    term_vec[ i ] = term_list

# Print term vectors with stop words removed

for vec in term_vec:
    print(vec)

```

```{python}
#exporting this as a database so we can play around with the base words
#characters_clean = Grouped_characters[['Main_Name','Season']]
#characters_clean['Dialogue'] = term_vec
#characters_clean.to_csv('cleaned_characters.csv', index = False)


```

# stemming 

```{python}
# Porter stem remaining terms
porter = nltk.stem.porter.PorterStemmer()

for i in range( 0, len( term_vec ) ):
    for j in range( 0, len( term_vec[ i ] ) ):
        term_vec[ i ][ j ] = porter.stem( term_vec[ i ][ j ] )

# Print term vectors with stop words removed

for vec in term_vec:
    print(vec)

```

# Creating a document term database for stemmed terms
```{python}
#creating a list of all unique terms sorted alphabetically
term_list = sorted(list({x for l in term_vec for x in l}))

#creating an empty frequency dataframe to serve as a counter for each term
#for each term vector
#ultimately creating a term matrix
n = len( term_list )
column_names = Grouped_characters['Main_Name'].astype(str)# + Grouped_characters['Season'].astype(str)
freq = pd.DataFrame(0, index=np.arange(n), columns=column_names)

#looping through each term vector and appending the values to the dataframe
for i in range( 0, len( term_vec ) ):
    for term in term_vec[i]:
        pos = list(term_list).index(term)
        freq.iloc[:,i][ pos ] += 1

freq.insert(0, 'terms', term_list) 

```

# looking at who says the others names the most
# network map
```{python}
character_references = freq[freq['terms'].isin(['elain', 'bania', 'georg','jerri', 'frank', 'mickey', 'kramer', 'newman', 'peterman',  'puddi', 'tim'])]

character_references['terms'] = ['bania', 'elaine', 'frank', 'george','jerry',  'kramer', 'newman', 'mickey', 'peterman',  'puddy', 'tim']

character_references = character_references.reset_index()
character_references.index.name = None
del character_references['index']

for character in character_references['terms']:
    if character in ['jerry', 'george', 'elaine', 'kramer']:
        continue
    else:
        character_references[character] = character_references[character] * 15

tempterms = character_references['terms']
del character_references['terms']

#take sums in upper triangle
for i in range(character_references.shape[0]):
    for j in range(len(character_references.columns)):
        if i < j:
            character_references.iloc[i, j] = character_references.iloc[i, j] + character_references.iloc[j, i]

#zero lower triangle
for i in range(character_references.shape[0]):
    for j in range(len(character_references.columns)):
        if i > j:
            character_references.iloc[i, j] = 0

character_references['terms'] = tempterms


net = Network()

# Adding nodes
for character in character_references['terms']:
    if character in ['jerry', 'george', 'elaine', 'kramer']:
        net.add_node(character, color = 'd7301f', font='20px arial black', shape = 'circle', physics = False, size = 50)
    else:
        net.add_node(character, shape = 'circle', physics = False, size = 3 )

# Adding edges
for i in range(len(character_references)):
    for j in range(1, len(character_references.columns)):
        if character_references.columns[j] == character_references['terms'][i]:
            continue
        elif character_references.iloc[i, j] > 0:
            net.add_edge(character_references['terms'][i], character_references.columns[j], value=float(character_references.iloc[i, j]))  # Convert to float

# Visualizing the network
net.save_graph('character_network_mirrored.html')


```

```{python} 
subset_columns = ['bania', 'elaine', 'frank', 'george','jerry',  'kramer', 'newman', 'mickey', 'peterman',  'puddy', 'tim']

character_references_2= character_references.copy()

character_references[subset_columns].sum()

for i in subset_columns:
    sum_temp = character_references[i].sum()
    character_references[i] = (character_references[i]/sum_temp)*1000

character_references = character_references_2

net = Network()

# Adding nodes
for character in character_references['terms']:
    net.add_node(character)

# Adding edges
for i in range(len(character_references)):
    for j in range(1, len(character_references.columns)):
        if character_references.iloc[i, j] > 0:
            net.add_edge(character_references['terms'][i], character_references.columns[j], weight = float(character_references.iloc[i, j]))

# Visualizing the network
net.save_graph('character_network_percentages.html')

```

# who has the most unique words as a percentage of what they say
```{python}

```

# sentiment score for each character
# need to split sentances here
```{python}
sentiment = SentimentIntensityAnalyzer()
pos_list = []
neu_list = []
neg_list = []
com_list = []

# Function to split dialogue by sentences
def split_sentences(dialogue):
    return nltk.sent_tokenize(dialogue)

main_characters_diag = main_characters[['Main_Name', 'Dialogue']].copy()

main_characters_diag['Dialogue_Sentences'] = main_characters_diag['Dialogue'].apply(split_sentences)

main_characters_diag = main_characters_diag.explode('Dialogue_Sentences').reset_index(drop=True)


for vec in main_characters_diag['Dialogue']:
    score = sentiment.polarity_scores( vec )
    pos_list.append(score['pos'])
    neu_list.append(score['neu'])
    neg_list.append(score['neg'])
    com_list.append(score['compound'])

main_characters_diag['Sentiment_pos'] = pos_list
main_characters_diag['Sentiment_neu'] = neu_list
main_characters_diag['Sentiment_neg'] = neg_list
main_characters_diag['Sentiment_com'] = com_list

sentiments = main_characters_diag.groupby(['Main_Name']).mean(['Sentiment_pos'])

#Kramer, Puddy, and Tim are more positive than anyone else 
#out of the main four Kramer is also the least negative

#visualize this if there is anything of interest
sentiments
#visualizing sentiment
data = sentiments.to_numpy()
plt.figure(figsize = (10,10))
plt.imshow(data, interpolation='nearest')
plt.xticks(np.arange(0, 1, step=0.2))  # Set label locations.
plt.xticks(np.arange(4), ['Positive', 'Neutral', 'Negative', 'Compound'])  # Set text labels.
plt.yticks(np.arange(0, 1, step=0.2))  # Set label locations.
plt.yticks(np.arange(11), ['bania','elaine', 'frank', 'george', 'jerry', 'kramer', 'mickey', 'newman','peterman', 'puddy','tim'])  # Set text labels.
plt.colorbar()
plt.savefig('Sentiment.png')

```

# document for raw terms
```{python}
#creating a list of all unique terms sorted alphabetically
raw_term_list = sorted(list({x for l in raw_terms for x in l}))

#creating an empty frequency dataframe to serve as a counter for each term
#for each term vector
#ultimately creating a term matrix
n = len( raw_term_list )
column_names = Grouped_characters['Main_Name'].astype(str)
freq_raw = pd.DataFrame(0, index=np.arange(n), columns=column_names)

#looping through each term vector and appending the values to the dataframe
for i in range( 0, len( raw_terms ) ):
    for term in raw_terms[i]:
        pos = list(raw_term_list).index(term)
        freq_raw.iloc[:,i][ pos ] += 1

freq_raw.insert(0, 'terms', raw_term_list) 
```

# similarity scores for each character for each season
```{python}
dict = gensim.corpora.Dictionary( term_vec )

#creating a corpus of text for this
corp = [ ]
for i in range( 0, len( term_vec ) ):
    corp.append( dict.doc2bow( term_vec[ i ] ) )

#  Create TFIDF vectors based on term vectors bag-of-word corpora

tfidf_model = gensim.models.TfidfModel( corp )

tfidf = [ ]
for i in range( 0, len( corp ) ):
    tfidf.append( tfidf_model[ corp[ i ] ] )

#  Create pairwise document similarity index

n = len( dict )
index = gensim.similarities.SparseMatrixSimilarity( tfidf_model[ corp ], num_features = n )

#  Print TFIDF vectors and pairwise similarity per document

for i in range( 0, len( tfidf ) ):
    s = 'Doc ' + str( i + 1 ) + ' TFIDF:'

    for j in range( 0, len( tfidf[ i ] ) ):
        s = s + ' (' + dict.get( tfidf[ i ][ j ][ 0 ] ) + ','
        s = s + ( '%.3f' % tfidf[ i ][ j ][ 1 ] ) + ')'

    print(s)

for i in range( 0, len( corp ) ):
    if i == 0:
        sim = pd.DataFrame(index[ tfidf_model[ corp[ i ] ] ])
        sim.rename(columns= {'0' : main_characters['Main_Name'].unique()[i]})
    else:
        sim[main_characters['Main_Name'].unique()[i]] = pd.DataFrame(index[ tfidf_model[ corp[ i ] ] ])

sim = sim.set_index(original_order)
sim.columns = original_order

# Custom sorting order
sorting_order = ['jerry', 'george', 'kramer', 'elaine', 'newman', 'frank', 'tim','mickey', 'bania', 'puddy', 'peterman']

# Reorder the DataFrame according to the custom sorting order
sim = sim.reindex(index=sorting_order, columns=sorting_order)

#visualizing similarity
data = sim.to_numpy()
plt.figure(figsize = (10,10))
plt.imshow(sim, interpolation='nearest')
plt.xticks(np.arange(0, 1, step=0.2))  # Set label locations.
plt.xticks(np.arange(11), ['jerry', 'george', 'kramer', 'elaine', 'newman', 'tim','frank','mickey','bania', 'puddy','peterman'])  # Set text labels.
plt.yticks(np.arange(0, 1, step=0.2))  # Set label locations.
plt.yticks(np.arange(11), ['jerry', 'george', 'kramer', 'elaine', 'newman', 'tim','frank','mickey','bania', 'puddy','peterman'])  # Set text labels.
plt.colorbar()
plt.savefig('Similarity.png')

#all 4 main characters are similar, Elaine is least simialr to other main characters

#kramer surprisingly is closest to other characters
#peterman is furthest from everything else, but closer to Kramer than anyone else


#looking at Tableau
#Jerry, Elaine, Kramer, and George's top 4 lines of dialogue are:
# What?, Yeah, Hey, No
```