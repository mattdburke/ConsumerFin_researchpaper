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

# paydayhomepagedirectory = os.fsencode("C:/Users/mattb/Documents/
	# University/PhD/Project_ConsumerFinance/Chapter_2/
	# Paydayhomepages/")

# path_paydayhome = "C:/Users/mattb/Documents/University/PhD/
	# Project_ConsumerFinance/Chapter_2/Paydayhomepages/"

# PersonalLoanhomepagedirectory = os.fsencode("C:/Users/mattb/
	# Documents/University/PhD/Project_ConsumerFinance/
	# Chapter_2/PersonalLoanhomepages/")

# path_PersonalLoanhomepage = "C:/Users/mattb/Documents/
	# University/PhD/Project_ConsumerFinance/Chapter_2/
	# PersonalLoanhomepages/"

# Creditdirectory = os.fsencode("C:/Users/mattb/Documents/
	# University/PhD/Project_ConsumerFinance/Chapter_2/
	# Credithomepages/")

# path_Credit = "C:/Users/mattb/Documents/University/PhD/
	# Project_ConsumerFinance/Chapter_2/Credithomepages/"


# # 5. We store the file names and directory paths in a vector for 
# # each category of lender, 
# # these can then easily be iterated through for analysis.

# PDHfiles = []
# for file in os.listdir(paydayhomepagedirectory):
	# PDHfiles.append(path_paydayhome+os.fsdecode(file))

# PLfiles = []
# for file in os.listdir(PersonalLoanhomepagedirectory):
	# PLfiles.append(path_PersonalLoanhomepage+os.fsdecode(file))
	
# Cfiles = []
# for file in os.listdir(Creditdirectory):
	# Cfiles.append(path_Credit+os.fsdecode(file))
	
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
	
# PDHtext = list(map(web_to_text, PDHfiles))

# PLtext = list(map(web_to_text, PLfiles))

# Ctext = list(map(web_to_text, Cfiles))

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

# PDHflesch2 = list(map(flesch2, PDHtext))

# PDHfleschkincaid = list(map(flesch_kincaid, PDHtext))

# PDHfogindex = list(map(fogindex, PDHtext))

# PDHwordcount = list(map(word_count, PDHtext))

# PDHsentcount = list(map(sentence_count, PDHtext))

# PDHcomplexwordcount = list(map(complex_word_counter, PDHtext))

# PDHapr = list(map(APR_explained_check, PDHtext))

# PDHfinanceterm = list(map(finance_terms, PDHtext))

# PDHfinancetocomplex = list(map(financial_to_complex, PDHtext))

# PDHsupercomplexwordcount = list(map(super_complex_word_counter, 
	# PDHtext))

# PDHsuperfinancetocomplex = list(map(super_financial_to_complex, 
	# PDHtext))


# PLflesch2 = list(map(flesch2, PLtext))

# PLfleschkincaid = list(map(flesch_kincaid, PLtext))

# PLfogindex = list(map(fogindex, PLtext))

# PLwordcount = list(map(word_count, PLtext))

# PLsentcount = list(map(sentence_count, PLtext))

# PLcomplexwordcount = list(map(complex_word_counter, PLtext))

# PLapr = list(map(APR_explained_check, PLtext))

# PLfinanceterm = list(map(finance_terms, PLtext))

# PLfinancetocomplex = list(map(financial_to_complex, PLtext))

# PLsupercomplexwordcount = list(map(super_complex_word_counter, 
	# PLtext))

# PLsuperfinancetocomplex = list(map(super_financial_to_complex, 
	# PLtext))


# Cflesch2 = list(map(flesch2, Ctext))

# Cfleschkincaid = list(map(flesch_kincaid, Ctext))

# Cfogindex = list(map(fogindex, Ctext))

# Cwordcount = list(map(word_count, Ctext))

# Csentcount = list(map(sentence_count, Ctext))

# Ccomplexwordcount = list(map(complex_word_counter, Ctext))

# Capr = list(map(APR_explained_check, Ctext))

# Cfinanceterm = list(map(finance_terms, Ctext))

# Cfinancetocomplex = list(map(financial_to_complex, Ctext))

# Csupercomplexwordcount = list(map(super_complex_word_counter, 
	# Ctext))

# Csuperfinancetocomplex = list(map(super_financial_to_complex, 
	# Ctext))

# # 11. Now we assign each of our variables to a PANDA

# import pandas as pd
# cat_PDH = [0] * len(PDHflesch2)
# cat_PL = [0] * len(PLflesch2)
# cat_C = [0] * len(Cflesch2)

# PDHdata = {'Flesch': PDHflesch2, 
		# 'FleschKincaid': PDHfleschkincaid,
		# 'FogIndex': PDHfogindex,
		# 'WordCount': PDHwordcount,
		# 'SentCount': PDHsentcount,
		# 'Category': cat_PDH,
		# 'ComplexCount': PDHcomplexwordcount,
		# 'Terms': PDHfinanceterm,
		# 'FinancetoComplex': PDHfinancetocomplex,
		# 'SuperComplexCount': PDHsupercomplexwordcount,
		# 'SuperFinancetoComplex': PDHsuperfinancetocomplex,
		# 'APRexplained': PDHapr}
# PDHFrame = pd.DataFrame(PDHdata)

# PLdata = {'Flesch': PLflesch2, 
		# 'FleschKincaid': PLfleschkincaid,
		# 'FogIndex': PLfogindex,
		# 'WordCount': PLwordcount,
		# 'Category': cat_PL,
		# 'SentCount': PLsentcount,
		# 'ComplexCount': PLcomplexwordcount,
		# 'Terms': PLfinanceterm,
		# 'FinancetoComplex': PLfinancetocomplex,
		# 'SuperComplexCount': PLsupercomplexwordcount,
		# 'SuperFinancetoComplex': PLsuperfinancetocomplex,
		# 'APRexplained': PLapr}
# PLFrame = pd.DataFrame(PLdata)

# Cdata = {'Flesch': Cflesch2, 
		# 'FleschKincaid': Cfleschkincaid,
		# 'FogIndex': Cfogindex,
		# 'WordCount': Cwordcount,
		# 'SentCount': Csentcount,
		# 'Category': cat_C,
		# 'ComplexCount': Ccomplexwordcount,
		# 'Terms': Cfinanceterm,
		# 'FinancetoComplex': Cfinancetocomplex,
		# 'SuperComplexCount': Csupercomplexwordcount,
		# 'SuperFinancetoComplex': Csuperfinancetocomplex,
		# 'APRexplained': Capr}
# CFrame = pd.DataFrame(Cdata)

# dataframe = pd.concat([PDHFrame, PLFrame, CFrame])

# dataframe.to_csv("data.csv")


text = """The Coronavirus Business Interruption Loan Scheme (CBILS) is available for SMEs through more than 40 accredited lenders across the UK.

Please read our CBILS FAQ for businesses

HOW TO APPLY
1.             FIND A LENDER
View and select a CBILS accredited lender

2.             APPROACH A LENDER
You should approach a lender yourself, ideally via the lender’s website.

Notes: There is high demand for CBILS facilities. Phone lines are likely to be busy and branches may not be able to handle enquiries in person.

Not every accredited lender can provide every type of finance available under CBILS, and the amount of finance offered varies between lenders. Please see the lenders’ websites for more information on the amounts they are able to offer.

3.             THE LENDER MAKES A DECISION
The lender has the authority to decide whether to offer you finance.

Under the scheme, lenders will not take personal guarantees of any form for facilities below £250,000.

For facilities above £250,000, personal guarantees may still be required, at a lender’s discretion, but:

they exclude the Principal Private Residence (PPR), and
recoveries under these are capped at a maximum of 20% of the outstanding balance of the CBILS facility after the proceeds of business assets have been applied
4.             IF THE LENDER TURNS YOU DOWN
If one lender turns you down, you can still approach other lenders within the scheme.

Access to the scheme has now been opened up to smaller businesses facing cashflow difficulties who previously would not have been eligible for CBILS because they met the requirements for a standard commercial facility.

You may therefore consider re-contacting your lender if you have previously been unsuccessful in securing funding.

WHO IS ELIGIBLE
Your business must:

Be UK-based in its business activity
Have an annual turnover of no more than £45 million
Have a borrowing proposal which the lender would consider viable, were it not for the current pandemic
Self-certify that it has been adversely impacted by the coronavirus (COVID-19).
View our Quick Eligibility Checklist

Businesses from any sector can apply, except the following:

Banks, insurers and reinsurers (but not insurance brokers)
Public-sector bodies
Further-education establishments, if they are grant-funded
State-funded primary and secondary schools
View our CBILS frequently asked questions for businesses

WHAT LENDERS WILL NEED FROM YOU
When you apply for a business loan, most lenders will ask you for the following:

DETAILS OF THE LOAN
The amount you would like to borrow
What the money is for — the lender will check that it’s a suitable business purpose and the right type of finance for your needs
The period over which you will make the repayments — the lender will assess whether the loan is affordable for you
SUPPORTING DOCUMENTS
You will need to provide certain evidence to show that you can afford to repay the loan. This is likely to include:

Management accounts
Cash flow forecast
Business plan
Historic accounts
Details of assets
The above requirements will vary from lender to lender. If you do not have everything listed here, a CBILS loan could still be an option to provide finance to support your business.

Note: For many customers approaching their existing lenders for a smaller facility, the process may be automated and therefore may not require the same level of documentation.

To learn more about lenders’ requirements, see the Better Business Finance lending application checklist.

OTHER BUSINESS FINANCE SUPPORT OPTIONS
You can find details of other government measures to support public services, people and businesses through this disruption on the Government’s Business Support website.

GUIDANCE AND SUPPORT FOR YOUR BUSINESS DURING THIS PERIOD OF UNCERTAINTY
The British Business Bank and The Institute of Chartered Accountants in England and Wales (ICAEW) have created new content to help businesses that are facing financial and operational challenges as a result of the coronavirus outbreak.

You can view this regularly updated guidance at the Business Finance Guide website."""


text2 = """ABOUT THE SCHEME
The Coronavirus Business Interruption Loan Scheme (CBILS) provides financial support to smaller businesses (SMEs) across the UK that are losing revenue, and seeing their cashflow disrupted, as a result of the COVID-19 outbreak.

The scheme is a part of a wider package of government support for UK businesses and employees. Read more at the Government’s Business Support website.

CBILS has been significantly expanded along with changes to the scheme’s features and eligibility criteria. The changes mean even more smaller businesses across the UK impacted by the coronavirus crisis can access the funding they need.

Importantly, access to the scheme has been opened up to those smaller businesses who would have previously met the requirements for a commercial facility but would not have been eligible for CBILS. Insufficient security is no longer a condition to access the scheme.

This significantly increases the number of businesses eligible for the scheme. The expanded scheme will be operational with lenders from Monday 6 April 2020.

HOW IT WORKS
British Business Bank operates CBILS via its accredited lenders. There are over 40 of these lenders currently working to provide finance. They include:

high-street banks
challenger banks
asset-based lenders
smaller specialist local lenders
A lender can provide up to £5 million in the form of:

term loans
overdrafts
invoice finance
asset finance
CBILS gives the lender a government-backed guarantee for the loan repayments to encourage more lending.

The borrower remains fully liable for the debt.

Under the scheme, personal guarantees of any form will not be taken for facilities below £250,000.

For facilities above £250,000, personal guarantees may still be required, at a lender’s discretion, but:

recoveries under these are capped at a maximum of 20% of the outstanding balance of the CBILS facility after the proceeds of business assets have been applied;
a Principal Private Residence (PPR) cannot be taken as security to support a personal guarantee or as security for a CBILS-backed facility
KEY FEATURES OF THE SCHEME
Finance of up to £5 million	Guarantee to the lender to encourage them to lend	Government pays interest and fees for 12 months
The maximum value of a facility provided under the scheme is £5 million, available on repayment terms of up to six years.	The scheme provides the lender with a government-backed, partial guarantee against the outstanding balance of the finance.

The borrower remains 100% liable for the debt.
The Government will make a Business Interruption Payment to cover the first 12 months of interest payments and any lender-levied charges.
Finance terms	Security	No guarantee fees for businesses
For term loans and asset finance facilities: up to six years.

For overdrafts and invoice finance facilities: up to three years.	Insufficient security is no longer a condition to access the scheme.

For all facilities, including those over £250,000, CBILS can now support lending to smaller businesses even where a lender considers there to be sufficient security, making more smaller businesses eligible to receive the Business Interruption Payment.

No personal guarantees for facilities under £250,000.

Personal guarantees may still be required, at a lender’s discretion, for facilities above £250,000, but they exclude the Principal Private Residence (PPR) and recoveries under these are capped at a maximum of 20% of the outstanding balance of the CBILS facility after the proceeds of business assets have been applied.
There are no guarantee fees for SMEs. Lenders pay a fee to access the scheme.
FIND OUT MORE
CBILS for SMEs – how smaller businesses can apply for the scheme

CBILS: current accredited lenders – lists of lenders already providing finance through CBILS

CBILS for accredited lenders – information for lenders on how to provide finance through CBILS

Become a CBILS accredited lender – details of accreditation for prosp"""


print (fogindex(text))

print (sentence_count(text))

print (word_count(text))

print (complex_word_counter(text))

#print (finance_terms(text))



print (fogindex(text2))

print (sentence_count(text2))

print (word_count(text2))

print (complex_word_counter(text2))

#print (finance_terms(text2))




text3 = """About this guidance
1. This guidance is intended to support Local Authorities in administering the
business grant schemes announced at Budget on 11 March 2020, and the
level of funding was increased in a statement from the Chancellor on 17
March. This guidance applies to England only.
2. This guidance sets out the criteria which central government considers for this
purpose to be eligible for the Small Business Grant Fund and the Retail,
Hospitality and Leisure Grant Fund. This does not replace existing guidance.
3. Local Authority enquiries on this measure should be addressed to
businessgrantfunds@beis.gov.uk. Businesses seeking information should
refer to the Government’s business support website:
https://www.businesssupport.gov.uk/
Introduction
4. In response to the Coronavirus, Covid – 19, the Government announced there
would be support for small businesses, and businesses in the retail,
hospitality and leisure sectors.
5. This support will take the form of two grant funding schemes in Financial Year
2020-2021, the Small Business Grant Fund and the Retail, Hospitality and
Leisure Grant Fund.
6. This document provides guidance to authorities about the operation and
delivery of the policy.
How will the grants be provided?
7. The Government will, in line with the eligibility criteria set out in this guidance,
reimburse Local Authorities that pay grants to eligible businesses. Central
government will fully reimburse Local Authorities, in line with the following
guidance and the grant offer letter sent to LAs, for the cost of the grant (using
a grant under section 31 of the Local Government Act 2003). Local
Authorities will be responsible for delivering the funding to eligible businesses.
8. We are committed to meeting the delivery costs to Local Authorities for this
scheme, and will meet associated New Burdens costs. A New Burdens
Assessment will be completed and funding then provided to authorities.
9. Local Authorities that will be responsible for making payments to businesses
and which will receive funding from Government are business rate billing
authorities in England.
10.This grant scheme will offer a lifeline to businesses who are struggling to
survive due to the corona virus shutdown. Local Authorities should make
payments as quickly as possible to support struggling businesses.
2
How much funding will be provided to businesses?
11.Under the Small Business Grant Fund (SBGF) all businesses in England in
receipt of either Small Business Rates Relief (SBRR) or Rural Rates Relief
(RRR) in the business rates system will be eligible for a payment of £10,000
in line with the eligibility criteria as set out in paragraphs 16-23.
12.Under the Retail, Hospitality and Leisure Grant (RHLG), businesses in
England that would have been in receipt of the Expanded Retail Discount
(which covers retail, hospitality and leisure) on 11 March with a rateable value
of less than £51,000 will be eligible for the following cash grants per property.
13.Eligible businesses in these sectors with a property that has a rateable value
of up to and including £15,000 will receive a grant of £10,000, in line with the
eligibility criteria as set out in paragraphs 24-31.
14.Eligible businesses in these sectors with a property that has a rateable value
of over £15,000 and less than £51,000 will receive a grant of £25,000, in line
with the eligibility criteria as set out in paragraphs 24-31.
15.Businesses with a rateable value of £51,000 or over are not eligible for this
scheme. Businesses which are not ratepayers in the business rates system
are not included in this scheme.
Who will benefit from these schemes?
Small Business Grant Fund
16.Hereditaments included in this scheme are those which on the 11 March 2020
were eligible for relief under the business rate Small Business Rate Relief
Scheme (including those with a Rateable Value between £12,000 and
£15,000 which receive tapered relief).
17.These are hereditaments to which:
a. Section 43 (4B)(a) of the Local Government Finance Act 1988 (small
business rate relief) applied, and
b. The value of E (as defined in article 3 of the Non-Domestic Rating
(Relies, thresholds and Amendment) (England) Order 2017, SI
2017/102) was greater than 1.
18.Hereditaments that were not eligible for percentage SBRR relief (including
those eligible for the Small Business Rate Multiplier) are excluded.
19.Hereditaments which on 11 March 2020 were eligible for relief under the rural
rate relief scheme are also eligible for this scheme. These are hereditaments
to which Section 43 (6B) of the Local Government Finance Act 1988 (rural
rate relief) applied.
3
20.Eligible recipients will be entitled to receive one grant per hereditament from
the earlier of the date payment of the grant by the Local Authority or 1st April
2020.
Exclusions to Small Business Grant Fund
21.Hereditaments occupied for personal uses. Examples of where there may be
personal use include private stables and loose boxes, beach huts and
moorings.
22.Car parks and parking spaces.
23.For the avoidance of doubt, businesses which as of the 11 March were in
liquation or were dissolved will not be eligible.
Retail, Hospitality and Leisure Grant
24. Hereditaments which on the 11 March 2020 had a rateable value of less than
£51,000 and would have been eligible for a discount under the business rates
Expanded Retail Discount Scheme had that scheme been in force for that
date are eligible for the grant. Charities which would otherwise meet this
criteria but whose bill for 11 March had been reduced to nil by a local
discretionary award should still be considered to be eligible for the RHL grant.
25.Eligible recipients will be entitled to receive one grant per hereditament from
the earlier of the date of payment of the grant by the Local Authority or 1st
April 2020.
Exclusions to RHLG
26.Recipients eligible for the Small Business Grant Fund will not be eligible for
the Retail, Hospitality and Leisure Grant.
27.Hereditaments occupied for personal uses. Examples of where there may be
personal use include private stables and loose boxes, beach huts and
moorings.
28.Car parks and parking spaces.
29.For the avoidance of doubt, businesses which as of the 11 March were in
liquation or were dissolved will not be eligible.
30.Hereditaments with a rateable value of £51,000 or over.
31.Only one grant may be awarded per hereditament.
Who will receive this funding?
32.The person who according to the billing authority’s records was the ratepayer
in respect of the hereditament on the 11 March 2020.
4
33.Where the Local Authority has reason to believe that the information that they
hold about the ratepayer on the 11 March 2020 is inaccurate they may
withhold or recover the grant and take reasonable steps to identify the correct
ratepayer. Local Authorities should make clear to recipients that the grant is
for the ratepayer and may be liable for recovery if the recipient was not the
ratepayer on the eligible day.
34.The Local Authority must call or write to the business, stating that by
accepting the grant payment, the business confirms that they are eligible for
the grant schemes. This includes where Local Authorities already have bank
details for businesses, and are in a position to send out funding immediately,
or where the Local Authority is sending a cheque to a business.
35.Landlords and management agents are urged to support local government in
quickly identifying the correct ratepayer.
Managing the risk of fraud
36.The Government will not accept deliberate manipulation and fraud - and any
business caught falsifying their records to gain additional grant money will
face prosecution and any funding issued will be subject to claw back, as may
any grants paid in error
37.The Government Grants Management Function and Counter Fraud Function
will make their digital assurance tool, Spotlight, available to Local Authorities,
and will offer support in using the tool and interpreting results. Alongside other
checks conducted by local authorities, the tool can help with pre-payment and
post payment assurance. We also want local authorities to work with us and
each other in identifying and sharing good practice, including protecting
eligible businesses which may be targeted by fraudsters pretending to be
central or local government or acting on their behalf.
Rating List Changes
38.Any changes to the rating list (rateable value or to the hereditament) after the
11 March 2020 including changes which have been backdated to this date
should be ignored for the purposes of eligibility.
39.Local Authorities are not required to adjust, pay or recover grants where the
rating list is subsequently amended retrospectively to the 11 March 2020.
Rating List Changes Exceptions
40.In cases where it was factually clear to the Local Authority on the 11 March
2020 that the rating list was inaccurate on that date, Local Authorities may
withhold the grant and/or award the grant based on their view of who would
have been entitled to the grant had the list been accurate.
41.This is entirely at the discretion of the Local Authority and only intended to
prevent manifest errors.
5
42.Where the Local Authority chooses to use this discretion then landlords and
managing agents are urged to support the Local Authority in identifying
quickly the correct ratepayers.
Post Event Assurance
43.Post payment, the Government Grants Management Function and Counter
Fraud Function will support local authorities to carry out post-event assurance
work to identify high risk payments.
Monitoring and Reporting Requirements
44.There will not be a mandatory application process for this scheme. However,
Local Authorities must retain necessary data provided and BEIS will
undertake regular data collection exercises. Further guidance will be
forthcoming to support this process. The data will include:
a) numbers of businesses eligible per scheme,
b) number of payments being processed per scheme, and
c) number of actual payments per scheme.
45.Annex B contains information on Post Payment Monitoring requirements.
State Aid
46.The United Kingdom left the EU on 31 January 2020, nonetheless under the
Withdrawal Agreement the State aid rules continue to apply during a transition
period, subject to regulation by the EU Commission. The Local Authority must
be satisfied that all State aid requirements have been fully met and complied
with when making grant payments, including, where required, compliance with
all relevant conditions of the EU State aid De-Minimis Regulation, the EU
Commission Temporary Framework for State aid measures to support the
economy in the current COVID-19 outbreak, the approved Covid-19
Temporary Framework for UK Authorities, and any relevant reporting
requirements to the EU Commission.
47.Payments made under SBGF can be provided under the existing De Minimis
rules, provided doing so does not exceed the €200,000 threshold. Payments
made under the RHLGF (or SBGF where the De Minimis threshold has been
reached) should be paid under the Covid-19 Temporary Framework for UK
Authorities.
48. Annex C of this guidance contains two sample declarations which local
authorities may wish to use with either payments under the De Minimis rules
or under the Covid-19 Temporary Framework for UK Authorities. Where Local
Authorities have further questions about De Minimis or other aspects of State
aid law, they."""








print (fogindex(text3))

print (sentence_count(text3))

print (word_count(text3))

print (complex_word_counter(text3))





