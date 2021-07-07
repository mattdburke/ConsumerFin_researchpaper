# Title: Project_ConsumerFinance.py

# Description: The aim of this program is to extract text from html
# files and then process it for readability to generate variables
# This is the program which generates the data used in the EL paper

# Author: Matt Burke

# Last modified: 26/04/2019

#1. This first section extracts financial words from the Harvey
# glossary.

import requests
from urllib.request import urlopen
import requests
import re
glos_str = "http://people.duke.edu/~charvey/classes/wpg/bfglos"
stri = ["a.htm",
		"b.htm",
		"c.htm",
		"d.htm",
		"e.htm",
		"f.htm",
		"g.htm",
		"h.htm",
		"i.htm",
		"j.htm",
		"k.htm",
		"l.htm",
		"m.htm",
		"n.htm",
		"o.htm",
		"p.htm",
		"q.htm",
		"r.htm",
		"s.htm",
		"t.htm",
		"u.htm",
		"v.htm",
		"w.htm",
		"x.htm",
		"y.htm",
		"z.htm"]
from bs4 import BeautifulSoup as bs		
html = []
for i in stri:
	html.append(glos_str+i)
financewords = []
for i in html:	
	request = requests.get(i)
	content = request.content
	soup = bs(content, 'html.parser')
	#p = str(soup.find_all('a', href=False))
	#a = re.compile('<.*?>')
	#b = re.sub(a, '', p)
	b = []
	for j in (soup.find_all('a', href=False)):
		b.append(j.get_text())
	financewords.extend(b)

# 2. We then want them in lower case, and stripped of blank spaces 
#either side.

financewords = [i.lower() for i in financewords]

financewords = [i.strip() for i in financewords]

# 3. We now turn to the bulk of the text analysis programming

import nltk
import re
import os
from nltk import word_tokenize as wt
from nltk.corpus import stopwords

from nltk import sent_tokenize as st
from curses.ascii import isdigit 


# 4. Extract the HTMLs from directory's on this PC,
# We define the paths to the files and additional path variables 
#for later use

paydayhomepagedirectory = os.fsencode("C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/Paydayhomepages/")

path_paydayhome = "C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/Paydayhomepages/"

PersonalLoanhomepagedirectory = os.fsencode("C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/PersonalLoanhomepages/")

path_PersonalLoanhomepage = "C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/PersonalLoanhomepages/"
    
Creditdirectory = os.fsencode("C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/Credithomepages/")

path_Credit = "C:/Users/mattb/Documents/GitHub/ConsumerFin_researchpaper/html/Credithomepages/"


# # 5. We store the file names and directory paths in a vector for 
# # each category of lender, 
# # these can then easily be iterated through for analysis.

PDHfiles = []
for file in os.listdir(paydayhomepagedirectory):
	PDHfiles.append(path_paydayhome+os.fsdecode(file))

PLfiles = []
for file in os.listdir(PersonalLoanhomepagedirectory):
	PLfiles.append(path_PersonalLoanhomepage+os.fsdecode(file))
	
Cfiles = []
for file in os.listdir(Creditdirectory):
	Cfiles.append(path_Credit+os.fsdecode(file))
	
# 6. This function extracts the text we are interested in from the 
# HTML, discards everything else.


def web_to_text(file):
	#text = []	
	with open(file, 'r', encoding='utf8') as markup:
		soup = bs(markup, 'html.parser')
		containers = soup.find_all('p')
		#for container in containers:
		text = [container.text.strip() for container in containers]
		#text.append(container.text.strip())
		text = " ".join(text)
		text = text.replace(u'\xa0',u' ')
		regex = re.compile(r'\n')
		text = (re.sub(regex, " ", text))
		return text

# 7. Here we store the text for each site in a vector. These 
# vectors are quite large given each 
# entry is a list of text from each site.
	
PDHtext = list(map(web_to_text, PDHfiles))

PLtext = list(map(web_to_text, PLfiles))

Ctext = list(map(web_to_text, Cfiles))

# 8. Here we define a range of functions to extract our variables 
# from the text. 
	
from string import punctuation
from nltk import sent_tokenize as st
from nltk.corpus import cmudict

# 9. We choose not to use a word tokenizer, since this captures 
# punctuation too, here we remove it all.

def word_count(text):
	stripped_punct_text = []
	a = re.findall(r"\w+(?:[-']\w+)*|'|[-.(]+|\S\w*", text)
	for i in a:
		if i not in punctuation:
			stripped_punct_text.append(i)
	return (len(stripped_punct_text))
	
from nltk.stem import WordNetLemmatizer
lemmatizer = WordNetLemmatizer()

def finance_terms(text):
	tokenized = wt(text)
	tokens = [i.lower() for i in tokenized]
	tokens = [word for word in tokens if word not in 
		stopwords.words('english')]
	tokens = [lemmatizer.lemmatize(i) for i in tokens]
	count = 0
	for i in tokens:
		if i in financewords:
			count = count + 1
	return count
	
def APR_explained_check(text):
	a = re.findall(r"annual percentage rate", text, 
		flags = re.IGNORECASE)
	return len(a)

def sentence_count(text):
	return len(st(text))

numsyllables_pronlist = lambda l: len(list(filter
	(lambda s: isdigit(s.encode('ascii', 'ignore').lower()[-1]), l)))
d = cmudict.dict()

def numsyllables(word):
	try:
		return list(set(map(numsyllables_pronlist, d[word.lower()])))
	except KeyError:
		return [0]

not_punctuation = lambda w: not (len(w)==1 and (not w.isalpha()))

def sup_syl(text):
	syllable_count = sum(map(lambda w: max(numsyllables(w)), 
		wt(text)))
	return syllable_count

def flesch2(text):
	wps = word_count(text)/sentence_count(text)
	spw = sup_syl(text)/word_count(text)
	return 206.835-1.015*wps-84.6*spw
	
def flesch_kincaid(text):
	wps = word_count(text)/sentence_count(text)
	spw = sup_syl(text)/word_count(text)
	return (0.39*wps)+(11.8*spw)-15.59
	
def fogindex(text):
	complexcounter = 0
	wps = word_count(text)/sentence_count(text)
	for i in wt(text):
		if sup_syl(i)>=3:
			complexcounter = complexcounter + 1
	cpw = complexcounter/word_count(text)
	return 0.4*((wps)+100*(cpw))
	
def complex_word_counter(text):
	complexcounter = 0
	for i in wt(text):
		if sup_syl(i)>=3:
			complexcounter = complexcounter + 1
	return complexcounter
	
def super_complex_word_counter(text):
	supercomplexcounter = 0
	for i in wt(text):
		if sup_syl(i)>=4:
			supercomplexcounter = supercomplexcounter + 1
	return supercomplexcounter

def financial_to_complex(text):
	tokenized = wt(text)
	tokens = [i.lower() for i in tokenized]
	tokens = [word for word in tokens if word not in 
		stopwords.words('english')]
	tokens = [lemmatizer.lemmatize(i) for i in tokens]
	counter = 0
	for x in tokens:
		if sup_syl(x)>=3:
			if x in financewords:
				counter = counter + 1
	return counter/complex_word_counter(text)
	
def super_financial_to_complex(text):
	tokenized = wt(text)
	tokens = [i.lower() for i in tokenized]
	tokens = [word for word in tokens if word not in 
		stopwords.words('english')]
	tokens = [lemmatizer.lemmatize(i) for i in tokens]
	counter = 0
	for x in tokens:
		if sup_syl(x)>=4:
			if x in financewords:
				counter = counter + 1
	return counter/super_complex_word_counter(text)
		
# 10. Now we iterate through all of the text lists and create 
# variables to form a PANDA

PDHflesch2 = list(map(flesch2, PDHtext))

PDHfleschkincaid = list(map(flesch_kincaid, PDHtext))

PDHfogindex = list(map(fogindex, PDHtext))

PDHwordcount = list(map(word_count, PDHtext))

PDHsentcount = list(map(sentence_count, PDHtext))

PDHcomplexwordcount = list(map(complex_word_counter, PDHtext))

PDHapr = list(map(APR_explained_check, PDHtext))

PDHfinanceterm = list(map(finance_terms, PDHtext))

PDHfinancetocomplex = list(map(financial_to_complex, PDHtext))

PDHsupercomplexwordcount = list(map(super_complex_word_counter, 
	PDHtext))

PDHsuperfinancetocomplex = list(map(super_financial_to_complex, 
	PDHtext))


PLflesch2 = list(map(flesch2, PLtext))

PLfleschkincaid = list(map(flesch_kincaid, PLtext))

PLfogindex = list(map(fogindex, PLtext))

PLwordcount = list(map(word_count, PLtext))

PLsentcount = list(map(sentence_count, PLtext))

PLcomplexwordcount = list(map(complex_word_counter, PLtext))

PLapr = list(map(APR_explained_check, PLtext))

PLfinanceterm = list(map(finance_terms, PLtext))

PLfinancetocomplex = list(map(financial_to_complex, PLtext))

PLsupercomplexwordcount = list(map(super_complex_word_counter, 
	PLtext))

PLsuperfinancetocomplex = list(map(super_financial_to_complex, 
	PLtext))


Cflesch2 = list(map(flesch2, Ctext))

Cfleschkincaid = list(map(flesch_kincaid, Ctext))

Cfogindex = list(map(fogindex, Ctext))

Cwordcount = list(map(word_count, Ctext))

Csentcount = list(map(sentence_count, Ctext))

Ccomplexwordcount = list(map(complex_word_counter, Ctext))

Capr = list(map(APR_explained_check, Ctext))

Cfinanceterm = list(map(finance_terms, Ctext))

Cfinancetocomplex = list(map(financial_to_complex, Ctext))

Csupercomplexwordcount = list(map(super_complex_word_counter, 
	Ctext))

Csuperfinancetocomplex = list(map(super_financial_to_complex, 
	Ctext))

# 11. Now we assign each of our variables to a PANDA

import pandas as pd
cat_PDH = [0] * len(PDHflesch2)
cat_PL = [0] * len(PLflesch2)
cat_C = [0] * len(Cflesch2)

PDHdata = {'Flesch': PDHflesch2, 
		'FleschKincaid': PDHfleschkincaid,
		'FogIndex': PDHfogindex,
		'WordCount': PDHwordcount,
		'SentCount': PDHsentcount,
		'Category': cat_PDH,
		'ComplexCount': PDHcomplexwordcount,
		'Terms': PDHfinanceterm,
		'FinancetoComplex': PDHfinancetocomplex,
		'SuperComplexCount': PDHsupercomplexwordcount,
		'SuperFinancetoComplex': PDHsuperfinancetocomplex,
		'APRexplained': PDHapr}
PDHFrame = pd.DataFrame(PDHdata)

PLdata = {'Flesch': PLflesch2, 
		'FleschKincaid': PLfleschkincaid,
		'FogIndex': PLfogindex,
		'WordCount': PLwordcount,
		'Category': cat_PL,
		'SentCount': PLsentcount,
		'ComplexCount': PLcomplexwordcount,
		'Terms': PLfinanceterm,
		'FinancetoComplex': PLfinancetocomplex,
		'SuperComplexCount': PLsupercomplexwordcount,
		'SuperFinancetoComplex': PLsuperfinancetocomplex,
		'APRexplained': PLapr}
PLFrame = pd.DataFrame(PLdata)

Cdata = {'Flesch': Cflesch2, 
		'FleschKincaid': Cfleschkincaid,
		'FogIndex': Cfogindex,
		'WordCount': Cwordcount,
		'SentCount': Csentcount,
		'Category': cat_C,
		'ComplexCount': Ccomplexwordcount,
		'Terms': Cfinanceterm,
		'FinancetoComplex': Cfinancetocomplex,
		'SuperComplexCount': Csupercomplexwordcount,
		'SuperFinancetoComplex': Csuperfinancetocomplex,
		'APRexplained': Capr}
CFrame = pd.DataFrame(Cdata)

dataframe = pd.concat([PDHFrame, PLFrame, CFrame])

dataframe.to_csv("data.csv")






