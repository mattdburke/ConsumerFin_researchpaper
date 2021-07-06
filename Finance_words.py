# Title: Finance_words.py

# Description: This program scrapes Campbell Harvey's Finance 
# Glossary and deposits the list into a csv file. 

# Author: Matt Burke

# Last modified: 08/12/2018

#1. This first section extracts financial words from the Harvey
# glossary.

import requests
from urllib.request import urlopen
import pandas as pd
from bs4 import BeautifulSoup as bs		
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

# 2. finance words with white space stripped either side.

financewords = [i.strip() for i in financewords]

data = {'Word': financewords}

frame = pd.DataFrame(data)

path = 'C:\\Users\\mattb\\Documents\\University\\PhD\\
	Project_NewsReadPrice\\'

frame.to_csv(path+"financewords.csv")

# 3. converted to lower case

financewords_lower = [i.lower() for i in financewords]

data = {'Word': financewords_lower}
frame = pd.DataFrame(data)

frame.to_csv(path+"financewords_lower.csv")

#4. Remove phrases

financewords = [i for i in financewords if " " not in i]

#5. Remove abbreviations

financewords = [i for i in financewords if (sum(1 for c in i if 
	c.isupper()))<=1]

financewords = [i for i in financewords if len(i)>1]

financewords = [i.lower() for i in financewords]

data = {'Word': financewords}

frame = pd.DataFrame(data)

frame.to_csv(path+"financewordsLM.csv")
















