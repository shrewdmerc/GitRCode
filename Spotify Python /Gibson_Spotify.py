# Name: Clay Gibson
# Date: 30 January 2015
# Description: Python Coding Assignment for Spotify
# File Path: ~/Dropbox/Senior Year/Term 2/Gibson_Spotify.py

import sys
import nltk
from nltk.tokenize import sent_tokenize
from nltk.tokenize import RegexpTokenizer
#from nltk.tree import *
#from nltk.draw import tree
#from pattern.en import parsetree
#import numpy
import spotipy

def main():
	my_text = sys.argv[0]
	my_sentences = preprocess(my_text)
	cached_data = {}
	sp = spotipy.Spotify()
	for x in my_sentences:
		force(x)
	

# Preprocess takes in a string and returns a list of sentences broken down 
# into word tokens using nltk
def preprocess(text_input):
	sentence_tokens = []
	tokenizer = RegexpTokenizer("[\w']+") #Won't break up contractions
	for x in sent_tokenize(text_input): 
		sentence_tokens.append(tokenizer.tokenize(x))
	return sentence_tokens
	
	#sentences = parsetree(text_input, relations = True, lemmata = True )
	#return sentences


# Force takes in tokenized sentence and then finds song titles within 
# using the following method: search full sentence. If no search result 
# match, search sentence without last word. continue until you find a 
# match or get down to a single word. once match found, do process on 
# rest of unmatched words. 
def force(token_input):
	length = len(token_input)
	y = length
	x = 0
	while (y >= x):
		frag = ' '.join(token_input[x:y]) #Combine tokens to form string
		frag = frag.lower() #Deal with case in dictionary; see line 70
		#print(frag)
		if frag in cached_data: #First check cached data
			res = (frag, cached_data[frag][0], cached_data[frag][1])
		else:
			res = search_for_title(frag)
		
		if res != 0:
			print_res(res)
			x = y
			y = length
		elif (x==y) and res == 0:
			x = y+1
			y = length
		else: # if can't find single word (maybe try searching related words)
			y = y - 1

# Search for a track in Spotify API, cache search results
# return 0 if not found, list if found.
def search_for_title(my_string):
	results = sp.search(q = 'track:'+ my_string, type='track', limit=40)
	items = results['tracks']['items']
	for i, t in enumerate(items):
		cached_data[t['name'].lower()] = (t['artists'][0]['name'], t['external_urls']['spotify'])
		if t['name'].lower() == my_string:
			return (t['name'], t['artists'][0]['name'], t['external_urls']['spotify'])
			break
	return 0

def print_res(res):
	print (' - '.join((res[0].title(),res[1],res[2])))
