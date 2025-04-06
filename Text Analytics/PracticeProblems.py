# -*- coding: utf-8 -*-
"""
Created on Wed Sep 27 19:01:40 2023

@author: thfar
"""

import spacy

nlp = spacy.load('en_core_web_sm')
doc = nlp("This is a sentence.")

#or 
import en_core_web_sm
nlp = en_core_web_sm.load()

doc = nlp("This is a sentence")

############################################
nlp = spacy.load("en_core_web_md")

txt = "On October 23, Apple CEO Tim Cook Unveiled the new iPad \
    Mini, fourth generation iPad with Retina display, new iMac, and \
        the 13-inch MacBook Pro with Retina display." 

doc = nlp(txt)

for NE in doc.ents:
    print(NE.text, ": ", NE.label_)
    
##########################################################################
################### Practice Problem 1 ####################################
###########################################################################

omam_txt = "Two men, dressed in denim jackets and trousers and wearing \
    black, shapeless hats, walk single-file down a path near the pool. \
        Both men carry blanket rolls — called bindles — on their shoulders.\
        The smaller, wiry man is George Milton. Behind him is Lennie Small, a \
            huge man with large eyes and sloping shoulders, walking at a gait\
                that makes him resemble a huge bear. When Lennie drops near \
                    the pool's edge and begins to drink like a hungry animal, \
    George cautions him that the water may not be good. This advice is necessary \
        because Lennie is retarded and doesn't realize the possible dangers. \
            The two are on their way to a ranch where they can get temporary work, \
        and George warns Lennie not to say anything when they arrive. Because \
            Lennie forgets things very quickly, George must make him repeat even \
                the simplest instructions. \
    Lennie also likes to pet soft things. In his pocket, he has a dead mouse \
    which George confiscates and throws into the weeds beyond the pond. Lennie \
    retrieves the dead mouse, and George once again catches him and gives Lennie \
    a lecture about the trouble he causes when he wants to pet soft things (they \
    were run out of the last town because Lennie touched a girl's soft dress, \
    and she screamed). Lennie offers to leave and go live in a cave, causing \
    George to soften his complaint and tell Lennie perhaps they can get him a \
    puppy that can withstand Lennie's petting. \
    As they get ready to eat and sleep for the night, Lennie asks George to \
    repeat their dream of having their own ranch where Lennie will be able to \
    tend rabbits. George does so and then warns Lennie that, if anything bad \
    happens, Lennie is to come back to this spot and hide in the brush. Before \
    George falls asleep, Lennie tells him they must have many rabbits of various \
    colors."
    
    
nlp = spacy.load( 'en_core_web_lg' )
t_nlp = nlp( omam_txt )
POS = [ (token.text,token.pos_) for token in t_nlp ]
print( POS )

import matplotlib.pyplot as plt
import numpy as np
import re
import spacy
from collections import Counter

def print_dict( d ):
    """Print frequency dictionary. Key is 'representation', v
    frequency of representation.

    Args:
      d (dict): Dictionary of (rep,freq) pairs
    """

    keys = list( d.keys() )
    keys.sort()

    for k in keys:
        print( f'{k}: {d[ k ]}; ', end='' )
    print( '' )


# Character representation, both w/ and w/o punctuation
# Convert to lower case, use regex to create a version with no punctuation

t = omam_txt.lower()
t_no_punc = re.sub( r'[^\w\s]', '', t )

# Create punc, no punc dictionaries to hold character frequencies

char_dict = dict( Counter( list( t ) ) )
char_dict_no_punc = dict( Counter( list( t_no_punc ) ) )

# Print results

print( 'Character frequency' )
print_dict( char_dict )

print( 'Character frequency w/o punctuation' )
print_dict( char_dict_no_punc )

# Plot as bar graph

char = list( char_dict.keys() )
char.sort()

freq = [ ]
for c in char:
    freq = freq + [ char_dict[ c ] ]

    # Add any character in punctuation dict but not in no punctuation dict w/freq of zero
    
    if c not in char_dict_no_punc:
        char_dict_no_punc[ c ] = 0
    
char_no_punc = list( char_dict_no_punc.keys() )
char_no_punc.sort()

freq_no_punc = [ ]
for c in char_no_punc:
    freq_no_punc = freq_no_punc + [ char_dict_no_punc[ c ] ]

X = np.arange( len( freq ) )
w = 0.35

fig = plt.figure( figsize=(10,5) )
ax = fig.add_axes( [ 0, 0, 1, 1 ] )
ax.bar( X + 0.00, freq, color='b', width=w, label='w/punc' )
ax.bar( X + 0.33, freq_no_punc, color='orange', width=w, label='w/o punc' )

plt.ylabel( 'Frequency' )
plt.xlabel( 'Character' )
plt.xticks( X + w / 2, char )
plt.legend( loc='best' )
plt.show()


#####################################################################
################### Practice Problem 2 ##############################
#######################################################################

import gensim
import nltk
import re
import string

doc = []
doc.append( 'It is a far, far better thing I do, than I have every done')
doc.append( 'Call me Ishmael')
doc.append( 'Is this a dagger I see before me?')
doc.append( 'O happy dagger')

punc = re.compile( '[%s]' % re.escape( string.punctuation))
term_vec = []

for d in doc:
    d = d.lower()
    d = punc.sub(' ', d)
    term_vec.append(nltk.word_tokenize( d ))

for vec in term_vec:
    print(vec)
    
####################### Part 2 - stopwords
stop_words = nltk.corpus.stopwords.words('english')

for i in range(0, len(term_vec)):
    term_list = []
    
    for term in term_vec[i]:
        if term not in stop_words:
            term_list.append(term)
    term_vec[i] = term_list
            
            
for vec in term_vec:
    print(vec)
    
########################### Porter Stemming
porter = nltk.stem.porter.PorterStemmer()

for i in range(0, len(term_vec)):
    for j in range(0, len(term_vec[i])):
        term_vec[i][j] = porter.stem(term_vec[i][j])

for vec in term_vec:
    print(vec)
    
 ################# here is the practice problem
 
import nltk
import re
import string

nltk.download( 'stopwords' )

#Of Mice and Men, Lord of the Flies, and 1984 text
txt = [
    'Two men, dressed in denim jackets and trousers and wearing "black, shapeless hats," walk single-file down a path near the pool. Both men carry blanket rolls  called bindles  on their shoulders. The smaller, wiry man is George Milton. Behind him is Lennie Small, a huge man with large eyes and sloping shoulders, walking at a gait that makes him resemble a huge bear. When Lennie drops near the pool\'s edge and begins to drink like a hungry animal, George cautions him that the water may not be good. This advice is necessary because Lennie is retarded and doesn\'t realize the possible dangers. The two are on their way to a ranch where they can get temporary work, and George warns Lennie not to say anything when they arrive. Because Lennie forgets things very quickly, George must make him repeat even the simplest instructions. Lennie also likes to pet soft things. In his pocket, he has a dead mouse which George confiscates and throws into the weeds beyond the pond. Lennie retrieves the dead mouse, and George once again catches him and gives Lennie a lecture about the trouble he causes when he wants to pet soft things (they were run out of the last town because Lennie touched a girl\'s soft dress, and she screamed). Lennie offers to leave and go live in a cave, causing George to soften his complaint and tell Lennie perhaps they can get him a puppy that can withstand Lennie\'s petting. As they get ready to eat and sleep for the night, Lennie asks George to repeat their dream of having their own ranch where Lennie will be able to tend rabbits. George does so and then warns Lennie that, if anything bad happens, Lennie is to come back to this spot and hide in the brush. Before George falls asleep, Lennie tells him they must have many rabbits of various colors.',
    'A fair-haired boy lowers himself down some rocks toward a lagoon on a beach. At the lagoon, he encounters another boy, who is chubby, intellectual, and wears thick glasses. The fair-haired boy introduces himself as Ralph and the chubby one introduces himself as Piggy. Through their conversation, we learn that in the midst of a war, a transport plane carrying a group of English boys was shot down over the ocean. It crashed in thick jungle on a deserted island. Scattered by the wreck, the surviving boys lost each other and cannot find the pilot. Ralph and Piggy look around the beach, wondering what has become of the other boys from the plane. They discover a large pink and cream-colored conch shell, which Piggy realizes could be used as a kind of makeshift trumpet. He convinces Ralph to blow through the shell to find the other boys. Summoned by the blast of sound from the shell, boys start to straggle onto the beach. The oldest among them are around twelve; the youngest are around six. Among the group is a boys choir, dressed in black gowns and led by an older boy named Jack. They march to the beach in two parallel lines, and Jack snaps at them to stand at attention. The boys taunt Piggy and mock his appearance and nickname. The boys decide to elect a leader. The choirboys vote for Jack, but all the other boys vote for Ralph. Ralph wins the vote, although Jack clearly wants the position. To placate Jack, Ralph asks the choir to serve as the hunters for the band of boys and asks Jack to lead them. Mindful of the need to explore their new environment, Ralph chooses Jack and a choir member named Simon to explore the island, ignoring Piggy\'s whining requests to be picked. The three explorers leave the meeting place and set off across the island. The prospect of exploring the island exhilarates the boys, who feel a bond forming among them as they play together in the jungle. Eventually, they reach the end of the jungle, where high, sharp rocks jut toward steep mountains. The boys climb up the side of one of the steep hills. From the peak, they can see that they are on an island with no signs of civilization. The view is stunning, and Ralph feels as though they have discovered their own land. As they travel back toward the beach, they find a wild pig caught in a tangle of vines. Jack, the newly appointed hunter, draws his knife and steps in to kill it, but hesitates, unable to bring himself to act. The pig frees itself and runs away, and Jack vows that the next time he will not flinch from the act of killing. The three boys make a long trek through dense jungle and eventually emerge near the group of boys waiting for them on the beach.',
    'On a cold day in April of 1984, a man named Winston Smith returns to his home, a dilapidated apartment building called Victory Mansions. Thin, frail, and thirty-nine years old, it is painful for him to trudge up the stairs because he has a varicose ulcer above his right ankle. The elevator is always out of service so he does not try to use it. As he climbs the staircase, he is greeted on each landing by a poster depicting an enormous face, underscored by the words "BIG BROTHER IS WATCHING YOU." Winston is an insignificant official in the Party, the totalitarian political regime that rules all of Airstrip One  the land that used to be called England  as part of the larger state of Oceania. Though Winston is technically a member of the ruling class, his life is still under the Party\'s oppressive political control. In his apartment, an instrument called a telescreen  which is always on, spouting propaganda, and through which the Thought Police are known to monitor the actions of citizens  shows a dreary report about pig iron. Winston keeps his back to the screen. From his window he sees the Ministry of Truth, where he works as a propaganda officer altering historical records to match the Partys official version of past events. Winston thinks about the other Ministries that exist as part of the Partys governmental apparatus: the Ministry of Peace, which wages war; the Ministry of Plenty, which plans economic shortages; and the dreaded Ministry of Love, the center of the Inner Partys loathsome activities. WAR IS PEACE FREEDOM IS SLAVERY IGNORANCE IS STRENGTH From a drawer in a little alcove hidden from the telescreen, Winston pulls out a small diary he recently purchased. He found the diary in a secondhand store in the proletarian district, where the very poor live relatively unimpeded by Party monitoring. The proles, as they are called, are so impoverished and insignificant that the Party does not consider them a threat to its power. Winston begins to write in his diary, although he realizes that this constitutes an act of rebellion against the Party. He describes the films he watched the night before. He thinks about his lust and hatred for a dark-haired girl who works in the Fiction Department at the Ministry of Truth, and about an important Inner Party member named O\'Brien  a man he is sure is an enemy of the Party. Winston remembers the moment before that days Two Minutes Hate, an assembly during which Party orators whip the populace into a frenzy of hatred against the enemies of Oceania. Just before the Hate began, Winston knew he hated Big Brother, and saw the same loathing in OBriens eyes. Winston looks down and realizes that he has written "DOWN WITH BIG BROTHER" over and over again in his diary. He has committed thoughtcrimethe most unpardonable crimeand he knows that the Thought Police will seize him sooner or later. Just then, there is a knock at the door.'
]




def porter_stem( txt ):
    """Porter stem terms in text block

    Args:
      txt (list of string): Text block as list of individual terms

    Returns:
      (list of string): Text block with terms Porter stemmed
    """

    porter = nltk.stem.porter.PorterStemmer()

    for i in range( 0, len( txt ) ):
        txt[ i ] = porter.stem( txt[ i ] )

    return txt


def remove_stop_word( txt ):
    """Remove all stop words from text blo
    Args:
      txt (list of string): Text block as list of individual terms

    Returns:
      (list of string): Text block with stop words removed
    """

    term_list = [ ]
    stop_word = nltk.corpus.stopwords.words( 'english' )

    for term in txt:
        term_list += ( [ ] if term in stop_word else [ term ] )

    return term_list


# Mainline

# Remove punctuation except hyphen

punc = string.punctuation.replace( '-', '' )
for i in range( 0, len( txt ) ):
    txt[ i ] = re.sub( '[' + punc + ']+', '', txt[ i ] )

# Lower-case and tokenize text

for i in range( 0, len( txt ) ):
    txt[ i ] = txt[ i ].lower().split()

# Stop word remove w/nltk stop word list, then Porter stem

for i in range( 0, len( txt ) ):
    txt[ i ] = remove_stop_word( txt[ i ] )
    txt[ i ] = porter_stem( txt[ i ] )

# Create list of all (unique) stemmed terms

term_list = set( txt[ 0 ] )
for i in range( 1, len( txt ) ):
    term_list = term_list.union( txt[ i ] )
term_list = sorted( term_list )

# Count occurrences of unique terms in each document

n = len( term_list )
freq = [ ]
for i in range( 0, len( txt ) ):
    freq.append( [ 0 ] * n )
    for term in txt[ i ]:
        pos = term_list.index( term )
        freq[ -1 ][ pos ] += 1

# Print transposed term-frequency list for easier viewing
print( '.....................mice..lord..1984' )
for i in range( 0, len( term_list ) ):
    print( '%20s:' % term_list[ i ], end='' )
    for j in range( 0, len( txt ) ):
        print( '%4d  ' % freq[ j ][ i ], end='' )
    print( '' )


#########################################################################
######################33 TF - IDF  ################################
###########################################################################

dict = gensim.corpora.Dictionary(term_vec)

corp = []
for i in range(0, len(term_vec)):
    corp.append(dict.doc2bow(term_vec[i]))
    
#create TFIDF vectors based on term vectors bag-of-word corpora
tfidf_model = gensim.models.TfidfModel(corp)

tfidf = []
for i in range(0, len(corp)):
    tfidf.append(tfidf_model[corp[i]])
    
#create pairwise document similarity index
n = len(dict)
index = gensim.similarities.SparseMatrixSimilarity(tfidf_model[corp], num_features = n)

#print TFIDF vectors and pairwise similarity per doucment

for i in range(0, len(tfidf)):
    s = 'Doc ' + str(i + 1) + ' TFIDF:'
    
    for j in range(0, len(tfidf[i])):
        s = s + ' (' + dict.get( tfidf[ i ][ j ][ 0 ] ) + ','
        s = s + ( '%.3f' % tfidf[ i ][ j ][ 1 ] ) + ')'
        
    print(s)
  
    ########3 this did not work for me
#for i in range( 0, len( corp ) ):
#    print ('Doc', ( i + 1 ), 'sim: [ ',

#    sim = index[ tfidf_model[ corp[ i ] ] ]
 #   for j in range( 0, len( sim ) ):
  #      print '%.3f ' % sim[ j ],
  
 # print ']'
 
 
 ##################################################################
 #################33 Latent Semantic Analysis #######################
 #########################################################################
 
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.decomposition import LatentDirichletAllocation

def display_doc_2_topic(doc_2_topic, collect):
    for i in range(0, len(collect)):
        topic_wt = list(doc_2_topic[i])
        idx = topic_wt.index(max(topic_wt))

        print(collect[i] + ":")
        print(f"  Topic {idx}, {topic_wt[ idx ] * 100.0:.02f}%")

def display_topics(model, feat_nm, top_word_n):
    for i, topic in enumerate(model.components_):
        print(f"Topic {i}:")
        topic_len = sum(topic)

        term = " ".join(
            [
                f"{feat_nm[i]} ({topic[i] / topic_len * 100.0:.02f}%); "
                for i in topic.argsort()[: -top_word_n - 1 : -1]
            ]
        )
        print("   " + term)

#  Mainline

collection = [
    "Romeo and Juliet",
    "Juliet, O happy dagger!",
    "Romeo died by a dagger",
    "'Live free or die', that's the New Hampshire motto",
    "Did you know that New Hampshire is in New England?",
]

feat_n = 10

#  Raw term counts for LDA

tf_vectorizer = CountVectorizer(
    max_df=0.95, min_df=2, max_features=feat_n, stop_words="english"
)
tf = tf_vectorizer.fit_transform(collection)
tf_feat_nm = tf_vectorizer.get_feature_names_out()

topic_n = 5
lda = LatentDirichletAllocation(
    n_components=topic_n,
    max_iter=5,
    learning_method="online",
    learning_offset=50.0,
    random_state=0,
)

lda_topic = lda.fit(tf)
doc_2_topic = lda.transform(tf)

top_word_n = 10
display_topics(lda, tf_feat_nm, top_word_n)

print()
display_doc_2_topic(doc_2_topic, collection)
 