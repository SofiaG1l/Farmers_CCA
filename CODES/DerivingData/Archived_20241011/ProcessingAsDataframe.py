# -*- coding: utf-8 -*-
"""
Created on Wed Jan 25 12:01:19 2023

@author: sgilclavel
"""

import os as os
import glob
from datetime import date

from tqdm import tqdm

# Saving Data into SQL
import sqlite3
import pickle

# Data Handling
import pandas as pd
import csv
pd.set_option('display.max_columns', 500) # Display any number of columns
pd.set_option('display.max_rows',500) # Display any number of rows

# Transform list representation into list
import ast 

# Network Analysis
import networkx as nx

# Basic XXX Libraries
import numpy as np
from collections import Counter
import random

# Processing text
import nltk as nltk
import regex as re
from bs4 import BeautifulSoup

# Import Text Processing Libraries
# import textacy as txt
# from textacy.extract import keyword_in_context as KWIC
import spacy as spacy
# Next is to tokenize
from spacy.tokenizer import Tokenizer
from spacy.util import compile_prefix_regex,compile_infix_regex, compile_suffix_regex
# Next is for the pipeline
# nlp = spacy.load('en_core_web_trf') # Small English language model

# Plots 
# from wordcloud import WordCloud
from matplotlib import pyplot as plt
# import seaborn as sns

# =============================================================================
# Functions
# =============================================================================
os.chdir("\\CODE\\Processing_PDFs\\")

import Functions as FN
import DataViz as DV
import Text_2_Net as NT2

''' Function to detect acronyms'''
# https://github.com/allenai/scispacy#abbreviationdetector

from scispacy.abbreviation import AbbreviationDetector

nlp = spacy.load("en_core_sci_sm")

# Add the abbreviation pipe to the spacy pipeline.
nlp.add_pipe("abbreviation_detector")

def CheckAbre(text):
    doc = nlp(text)
    ABR=pd.DataFrame(columns=["ABR","DEF"])
    for abrv in doc._.abbreviations:
        # break
        DEF=''.join(re.findall(r'[a-z\s]',str(abrv._.long_form)))
        DEF=DEF.strip()
        ABR=pd.concat([ABR,
                       pd.DataFrame({"ABR":str(abrv).strip(),"DEF":DEF}, index=[0])],\
                    ignore_index=True)
        # print(f"{abrv} \t ({abrv.start}, {abrv.end}) {abrv._.long_form}")
        ABR=ABR.drop_duplicates()
        ABR=ABR.reset_index(drop=True)
    return(ABR)

# =============================================================================
# Openning and cleaning the text
# =============================================================================


DIR="\\DATA\\PDFs_Clusters\\TXT\\" # !!!

FOLDERS=os.listdir(DIR)

df=pd.DataFrame(columns=["file","original","doi","TXT","introduction",
                "background","review","framework","methodology","analysis","results",
                "findings","conclusions","discussion","bibliography","references"]) 

count=0
for efe in FOLDERS:
    FILES=os.listdir(DIR+efe)
    # break
    for fi in tqdm(FILES):
        # break
        try:
            with open(DIR+efe+"\\"+fi,encoding="utf-8") as ewe:
                TXT=ewe.read()
            TXT=TXT.lower()
            TXT=TXT.replace("-\n","")
            # TXT=FN.FixingSpaces(TXT)
            
            # Replacing abreviations with meanings
            ABR=CheckAbre(TXT)
            for ii in range(ABR.shape[0]):
                # break
                abr=ABR.iloc[ii].ABR
                mnn=ABR.iloc[ii].DEF
                TXT=re.sub(r'(?:^|\W)'+abr+'(?:$|\W)'," "+mnn+" ",TXT)
            
            sections={"references":['r(\s|)e(\s|)f(\s|)e(\s|)r(\s|)e(\s|)n(\s|)c(\s|)e'],
                      "bibliography":['b(\s|)i(\s|)b(\s|)l(\s|)i(\s|)o(\s|)g(\s|)r(\s|)a(\s|)p(\s|)h'],
                      "discussion":['d(\s|)i(\s|)s(\s|)c(\s|)u(\s|)s(\s|)s(\s|)i(\s|)o(\s|)n'],
                      "conclusions":['c(\s|)o(\s|)n(\s|)c(\s|)l(\s|)u(\s|)s(\s|)'], # sion
                      "findings":['f(\s|)i(\s|)n(\s|)d(\s|)i(\s|)n(\s|)g(\s|)s'],
                      "results":['r(\s|)e(\s|)s(\s|)u(\s|)l(\s|)t'],
                      "analysis":['a(\s|)n(\s|)a(\s|)l(\s|)y(\s|)s'],
                      "methodology":['m(\s|)e(\s|)t(\s|)h(\s|)o(\s|)d'],
                      "framework":['f(\s|)r(\s|)a(\s|)m(\s|)e(\s|)w(\s|)o(\s|)r(\s|)k'],
                      "review":['l(\s|)i(\s|)t(\s|)e(\s|)r(\s|)a(\s|)t(\s|)u(\s|)r(\s|)e(\s|)r(\s|)e(\s|)v(\s|)i(\s|)e(\s|)w'],
                      "background":['b(\s|)a(\s|)c(\s|)k(\s|)g(\s|)r(\s|)o(\s|)u(\s|)n(\s|)d'],
                      "introduction":['i(\s|)n(\s|)t(\s|)r(\s|)o(\s|)d(\s|)u(\s|)c'],
                      "TXT":['t(\s|)h(\s|)e(\s|)o(\s|)r']}
            
            # Second checking whether the heading is between one \n
            HEADINGS=re.findall(r'(?<=\n\n)(.*)(?=\n)', TXT)
            HEADINGS_OR=HEADINGS.copy()
            HEADINGS=[(ehe.split(":"))[0] for ehe in HEADINGS]
            
            TEMP=[]
            LOCT=[] # we need to locate the original header to later use it
            for ece,ehe in enumerate(HEADINGS):
                if len(ehe)>5 and len(ehe)<30:
                    TEMP.append(ehe)
                    LOCT.append(ece)
            
            HEADINGS=TEMP.copy()
            HEADINGS_OR=[HEADINGS_OR[ece] for ece in LOCT]
            
            HEADINGS2=[]
            LOCT=[]
            for k in sections.keys():
                # break
                for ece,ehe in enumerate(HEADINGS):
                    # break
                    WHERE= re.search(sections[k][0],ehe) #list(map(ehe.find, sections[k]))
                    if type(WHERE)!=type(None):
                        # break
                        HEADINGS2.append(ehe) # sections[k][val[0]]
                        sections[k]=HEADINGS_OR[ece] #ehe
                        LOCT.append(ece)
                        break
            
            for k,v in sections.items():
                if type(v)==list:
                    sections[k]=""
                    
            # Checking that discussion and conclusions do not repeat
            if sections['discussion']==sections['conclusions']:
                sections['conclusions']=''
                
            ROW={}
            ROW[df.columns[0]]=re.sub(".txt","",fi) # cluster
            # TXT=re.sub('\W+',' ',TXT).strip() # Removes invisible chars
            original=re.split(r''+sections['references']+'',TXT)[0]
            ROW[df.columns[1]]=FN.tokenize(original) # text
            try:
                ROW[df.columns[2]]=re.findall(r'(?<=doi)(.*)(?=[\n|\s])', TXT)[0] # doi
            except:
                ROW[df.columns[2]]=fi[:-4]
            COUNT=len(df.columns)
            NUM_SECTIONS=0
            for k,v in sections.items():
                if len(v)>0:
                    NUM_SECTIONS+=1
                    SE=v
                    try:
                        SPLIT=re.split(r'(?<=\n\n.*)('+SE+')(?=\n)', TXT)
                        TXT=SPLIT[0]
                        if len(SPLIT)>1:
                            ROW[df.columns[COUNT-1]]=[SPLIT[2]]
                    except:
                        print(SE)
                COUNT-=1
            
            ROW["TXT"]=re.sub('\W+',' ',TXT).strip()
            
            df=pd.concat([df,pd.DataFrame([ROW])])
            
            count+=1
            
        except:
            count+=1


# Cleaning each column
for i in df.columns[3:]:
    try:
        df[i]=df[i].apply(lambda x: x.replace("\n\n"," ") if not pd.isna(x) else '')
        
    except:
        df[i]=df[i].apply(lambda x: x[0].replace("\n\n"," ") if not pd.isna(x) else '')

    df[i]=df[i].apply(lambda x: x.replace("\n"," "))
    df[i]=df[i].apply(lambda x: x.replace("ﬃ","ffi"))
    df[i]=df[i].apply(lambda x: x.replace("ﬀ","ff"))
    df[i]=df[i].apply(lambda x: x.replace("ﬁ","fi"))
    df[i]=df[i].apply(lambda x: x.replace("’","'"))
    df[i]=[re.sub("[^0-9a-z'\(\).,;:\-]+"," ",kk) for kk in df[i]] # removing special characters
    df[i]=[re.sub("\([a-z\s\.]+(,|\s)(;|,|\s|)(,|\s|)[0-9\s\,]+\)","",kk) for kk in df[i]] # removing citations
    df[i]=[re.sub("\(([a-z\s\.]+(,|\s)(;|,|\s|)(,|\s|)[0-9\s\,]+(;|\&|(\band\b)|))+\)","",kk) for kk in df[i]] # removing citations
    df[i]=[re.sub("et al(.|)(\s|)\([0-9]+\)","",kk) for kk in df[i]] # removing et al. (YEAR)
    df[i]=[re.sub(r'(?<=[.,:;])(?=[^\s])', r' ', kk) for kk in df[i]] # Adding space when missing
    
# =============================================================================
# Cleaning data to merge
# =============================================================================

SCOPUS=pd.read_csv("\DATA\Articles_Supervised.csv")

SCOPUS=SCOPUS[SCOPUS.included==1]

SCOPUS=SCOPUS.reset_index(drop=True)

SCOPUS.head()

df["checked"]=0
df["dc:identifier"]=""

## Searching by doi:
for cee in range(SCOPUS.shape[0]):
    
    ii=SCOPUS["doi"].iloc[cee]
    if not pd.isna(ii):
        LOC=[[ii,ce,its] for ce,its in enumerate(df.doi) if its.rfind(ii)!=-1]
        
        if len(LOC)>0:
            df["doi"].iloc[LOC[0][1]]=LOC[0][0]
            df["dc:identifier"].iloc[LOC[0][1]]=SCOPUS["dc:identifier"].iloc[cee]
            df["checked"].iloc[LOC[0][1]]=1

# =============================================================================
# Merging with Metadata
# =============================================================================

df2=df[df.checked==1]

df2=df2.merge(SCOPUS,how="left",on=["dc:identifier","doi"])

np.unique(df2.cluster)

df2['SUBJAREA_2'].value_counts().plot(kind='barh').invert_yaxis()

df2[['SUBJAREA_2','prism:publicationName']].groupby(['SUBJAREA_2']).\
    nunique().plot(kind='barh').invert_yaxis()

df2['cluster'].value_counts().plot(kind='barh').invert_yaxis()

# =============================================================================
# Cleaning Abstract
# =============================================================================

# Removing journals mark
df2['abstract']=df2['abstract'].apply(lambda x: '' if pd.isna(x) else x)
df2['abstract']=[re.sub("(\s|)[^0-9a-z\(\).,;:-]+"," ",kk.lower()) for kk in df2['abstract']] # removing special characters

df2['abstract']=[re.sub("^[a-z0-9,\s]* ag.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("^[a-z0-9,\s]* ltd(\.|)","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("^[a-z0-9,\s]* b.v.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("^[a-z0-9,\s]* llc.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 

df2['abstract']=[re.sub("^[a-z0-9,\s\(\)\.]* the author(\(|)(s|)(\)|)(\.|)","",kk) for kk in df2['abstract']] # 2021 the author(s)
df2['abstract']=[re.sub("^[a-z0-9,\s\(\)\.]* the author(\(|)(s|)(\)|) [0-9]+.","",kk) for kk in df2['abstract']] # 2021 the author(s)
df2['abstract']=[re.sub("^[a-z0-9,\s\.]* all rights reserved.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 

df2['abstract']=[re.sub("^[a-z0-9,\s\.]* sage [a-z0-9,\s]*.","",kk) for kk in df2['abstract']]
df2['abstract']=[re.sub("^[a-z0-9,\s\.]* wiley [a-z0-9,\s]*.","",kk) for kk in df2['abstract']]
df2['abstract']=[re.sub("^[a-z0-9,\s\.]*elsevier gmbh","",kk) for kk in df2['abstract']]
df2['abstract']=[re.sub("^[a-z0-9,\s\.]*elsevier [a-z0-9,\s]*.","",kk) for kk in df2['abstract']]
df2['abstract']=[re.sub("^[a-z0-9,\s\.]* springer[a-z0-9,\-\s]*.","",kk) for kk in df2['abstract']] # 2021, springer nature switzerland ag.
df2['abstract']=[re.sub("^[a-z0-9,\s\.]* francis group[a-z0-9,\s]*.","",kk) for kk in df2['abstract']] # 2021, springer nature switzerland ag.
df2['abstract']=[re.sub("^[a-z0-9,\s\.]* taylor francis[a-z0-9,\s]*.","",kk) for kk in df2['abstract']] # 2021, springer nature switzerland ag.
df2['abstract']=[re.sub("^[a-z0-9,\s]* all rights reserved.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("^[a-z0-9,\s\.]*copyright[a-z0-9,\-\s]*.","",kk) for kk in df2['abstract']] # 2021, springer nature switzerland ag.
df2['abstract']=[re.sub("^[a-z0-9,\s\.]*published[a-z0-9,\-\s]*.","",kk) for kk in df2['abstract']] # 2021, springer nature switzerland ag.
df2['abstract']=[re.sub("^[a-z0-9,\s\.]*open access[a-z0-9,\-\s]*.","",kk) for kk in df2['abstract']]

df2['abstract']=[re.sub("(\s|)licensee mdpi(,|) basel(,|) switzerland.","",kk) for kk in df2['abstract']] # 2021 by the author(s)
df2['abstract']=[re.sub("(\s|)[0-9]+(,|) emerald publishing limited(\.|\s|)purpose(\.|:|\s|)","",kk) for kk in df2['abstract']] # 2018, emerald publishing limited.purpose:
df2['abstract']=[re.sub("copyright [0-9]+ gammage and jarre.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("e.c.h. keskitalo and b.l. preston [0-9]+.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 
df2['abstract']=[re.sub("[0-9]+ castrej n, charles.","",kk) for kk in df2['abstract']] # 2019 elsevier ltd 

df2['abstract']=[re.sub("[0-9,\s]+ american [a-z]* of [a-z\s]*(\.|\s|)","",kk) for kk in df2['abstract']] 
df2['abstract']=[re.sub("[0-9,\s]+ hong kong [a-z]* of [a-z\s]*(\.|\s|)","",kk) for kk in df2['abstract']] 
df2['abstract']=[re.sub("(\s|)[0-9]+ international institute for environment and development (iied)(\.|\s|)","",kk) for kk in df2['abstract']] 
df2['abstract']=[re.sub("iwa publishing [0-9]+.","",kk) for kk in df2['abstract']] 
df2['abstract']=[re.sub("^[a-z0-9,\s]* by iwmi.","",kk) for kk in df2['abstract']] 
df2['abstract']=[re.sub("[0-9,]+ newcastle university.","",kk) for kk in df2['abstract']] 

df2['abstract']=[re.sub("^[0-9\s]+","",kk) for kk in df2['abstract']] # 2019 elsevier ltd
df2['abstract']=[re.sub(r'(?<=[.,:;])(?=[^\s])', r' ', kk) for kk in df2['abstract']] # Adding space when missing


# =============================================================================
# Tokenizing some of the sentences
# =============================================================================

nlp = spacy.load('en_core_web_trf')
nlp.tokenizer = FN.custom_tokenizer(nlp)


doc = nlp(str(df2.abstract[0]))
for token in doc:
    print(token, end="|")

# It is not Stop Word:
[t for t in doc if not t.is_stop and not t.is_punct]

patterns = ["POS:ADJ POS:NOUN:+"]


nlp_columns = list(FN.extract_nlp(nlp.make_doc('')).keys())
print(nlp_columns)

# Renaming from the df abstracts
N=df2.shape[0]
batch_size = 100

#### Tile
for col in nlp_columns:
    df2[col] = None

for i in tqdm(range(0, N, batch_size)):
    docs = nlp.pipe(df2['title'][i:i+batch_size])

    for j, doc in enumerate(docs):
        for col, values in FN.extract_nlp(doc).items():
            df2[col].iloc[i+j] = values

# Renaming from the df Conclusions
df2=df2.rename(columns={'lemmas':'lemmasTIT','adjs_verbs':'adjs_verbsTIT',
                    'nouns':'nounsTIT','noun_phrases':'noun_phrasesTIT',
                    'adj_noun_phrases':'adj_noun_phrasesTIT',
                    'entities':'entitiesTIT','sentence':'sentenceTIT'})

#### Abstracts
for col in nlp_columns:
    df2[col] = None

for i in tqdm(range(0, N, batch_size)):
    docs = nlp.pipe(df2['abstract'][i:i+batch_size])

    for j, doc in enumerate(docs):
        for col, values in FN.extract_nlp(doc).items():
            df2[col].iloc[i+j] = values

# Renaming from the df Conclusions
df2=df2.rename(columns={'lemmas':'lemmasABS','adjs_verbs':'adjs_verbsABS',
                    'nouns':'nounsABS','noun_phrases':'noun_phrasesABS',
                    'adj_noun_phrases':'adj_noun_phrasesABS',
                    'entities':'entitiesABS','sentence':'sentenceABS'})

# with open('/PROCESSED/df2.pickle', 'wb') as handle:
#     pickle.dump(df2, handle, protocol=pickle.HIGHEST_PROTOCOL)

# with open('/PROCESSED/df2.pickle', 'rb') as handle:
#     df2 = pickle.load(handle)

#### Introduction
for col in nlp_columns:
    df2[col] = None

for i in tqdm(range(0, N, batch_size)):
    docs = nlp.pipe(df2['introduction'][i:i+batch_size])

    for j, doc in enumerate(docs):
        for col, values in FN.extract_nlp(doc).items():
            df2[col].iloc[i+j] = values

# Renaming from the df Conclusions
df2=df2.rename(columns={'lemmas':'lemmasINT','adjs_verbs':'adjs_verbsINT',
                    'nouns':'nounsINT','noun_phrases':'noun_phrasesINT',
                    'adj_noun_phrases':'adj_noun_phrasesINT',
                    'entities':'entitiesINT','sentence':'sentenceINT'})

#### Conclusions
for col in nlp_columns:
    df2[col] = None

for i in tqdm(range(0, N, batch_size)):
    docs = nlp.pipe(df2['conclusions'][i:i+batch_size])

    for j, doc in enumerate(docs):
        for col, values in FN.extract_nlp(doc).items():
            df2[col].iloc[i+j] = values

# Renaming from the df Conclusions
df2=df2.rename(columns={'lemmas':'lemmasCON','adjs_verbs':'adjs_verbsCON',
                    'nouns':'nounsCON','noun_phrases':'noun_phrasesCON',
                    'adj_noun_phrases':'adj_noun_phrasesCON',
                    'entities':'entitiesCON','sentence':'sentenceCON'})

# =============================================================================
# Adding descriptive columns
# =============================================================================

#### Affiliation Country ####

df2['affiliation']=df2.affiliation.apply(lambda x: ast.literal_eval(x) if\
                                         not pd.isna(x) else '')

df2.affiliation.iloc[3][0]['affiliation-country']

[ii[0]['affiliation-country'] for ii in df2.affiliation if type(ii)==list]


AFF=[]
countries=Counter()
for ii in df2.affiliation:
    
    if type(ii)==list:
        AFF0=[]
        n=len(ii)
        
        for ee in range(n):
            AFF0.append(ii[ee]['affiliation-country'])
        
        countries.update(AFF0)            
        AFF.append(AFF0)
    
    else:
        AFF.append([])

df2['affiliation_Country']=AFF


#### Studied Countries ####

CITIES=pd.read_csv("C:/Dropbox/TU_Delft/Projects/Farmers_CCA/DATA/world-cities_csv.csv")

for cc in CITIES.columns[:-1]:
    CITIES[cc]=CITIES[cc].str.lower()

# CITIES.subcountry=CITIES.apply(lambda x: x.name 
#                                if pd.isna(x.subcountry) else x.subcountry, axis=1)

[ii for jj in df2['entitiesTIT'] for ii in jj if ii.find("/GPE")>-1]

STD_TIT=[]
STD_ABS=[]
STD_CON=[]

Country_Study=Counter()

n=df2.shape[0]
for ii in range(n):
    # break
    TIT=df2.iloc[ii]["entitiesTIT"]
    ABS=df2.iloc[ii]["entitiesABS"]
    CON=df2.iloc[ii]["entitiesCON"]
    
    if type(TIT)==list:
        STD0=[jj.replace("/GPE","") for jj in TIT if jj.find("/GPE")>-1]
        STD0=[jj.replace("_"," ") for jj in STD0]
        Country_Study.update(STD0)            
        STD_TIT.append(STD0)
    
    else:
        STD_TIT.append([])
    
    if type(ABS)==list:
        STD0=[jj.replace("/GPE","") for jj in ABS if jj.find("/GPE")>-1]
        STD0=[jj.replace("_"," ") for jj in STD0]
        Country_Study.update(STD0)            
        STD_ABS.append(STD0)
    
    else:
        STD_ABS.append([])

    if type(CON)==list:
        STD0=[jj.replace("/GPE","") for jj in CON if jj.find("/GPE")>-1]
        STD0=[jj.replace("_"," ") for jj in STD0]
        Country_Study.update(STD0)             
        STD_CON.append(STD0)
    
    else:
        STD_CON.append([])

df2["StudiedPlace"]=[STD_TIT[ii]+STD_ABS[ii] for ii in range(len(STD_TIT))]

NON=[n for n,i in enumerate(df2.StudiedPlace) if len(i)==0]

len(NON)

df2["StudiedPlace"].iloc[NON]=[STD_CON[i] for i in NON]

NON=[n for n,i in enumerate(df2.StudiedPlace) if len(i)==0]

len(NON)

#### Dictionary of those places that were not found:
STD_TIT=[]
STD_ABS=[]
STD_CON=[]

Country_Study=Counter()

n=df2.shape[0]
for ii in range(n):
    place=df2["StudiedPlace"].iloc[ii]
    
    if len(place)==0:
        TIT=df2.title.iloc[ii]
        ABS=df2.abstract.iloc[ii]
        CON=df2.apply(lambda x: x.conclusions+x.discussion,axis=1).iloc[ii]
        
        if len(TIT)>4:
            TIT1=np.where([len(jj)>0 for jj in [re.findall(r'\b' + jj + r'\b',TIT) for jj in list(CITIES.country)]])
            TIT2=np.where([len(jj)>0 for jj in [re.findall(r'\b' + str(jj) + r'\b',TIT) for jj in list(CITIES.subcountry)]])
            TITall=TIT1[0].tolist()+TIT2[0].tolist()
            if(len(TITall)>0):
                SUB=CITIES.iloc[TITall].copy()
                SUB=Counter(SUB.country).most_common(1)
                STD_TIT.append([SUB[0][0]])
            else:
                STD_TIT.append([])
        else:
            STD_TIT.append([])
        
        if len(ABS)>4:
            ABS1=np.where([len(jj)>0 for jj in [re.findall(r'\b' + jj + r'\b',ABS) for jj in list(CITIES.country)]])
            ABS2=np.where([len(jj)>0 for jj in [re.findall(r'\b' + str(jj) + r'\b',ABS) for jj in list(CITIES.subcountry)]])
            ABSall=ABS1[0].tolist()+ABS2[0].tolist()
            if(len(ABSall)>0):
                SUB=CITIES.iloc[ABSall].copy()
                SUB=Counter(SUB.country).most_common(1)
                STD_ABS.append([SUB[0][0]])
            else:
                STD_ABS.append([])
        else:
            STD_ABS.append([])
    
        if len(CON)>4:
            CON1=np.where([len(jj)>0 for jj in [re.findall(r'\b' + jj + r'\b',CON) for jj in list(CITIES.country)]])
            CON2=np.where([len(jj)>0 for jj in [re.findall(r'\b' + str(jj) + r'\b',CON) for jj in list(CITIES.subcountry)]])
            CONall=CON1[0].tolist()+CON2[0].tolist()
            if(len(CONall)>0):
                SUB=CITIES.iloc[CONall].copy()
                SUB=Counter(SUB.country).most_common(1)
                STD_CON.append([SUB[0][0]])
            else:
                STD_CON.append([])
        else:
            STD_CON.append([])
    else:
        STD_TIT.append(place)
        STD_ABS.append(place)
        STD_CON.append(place)
     
STD_TIT=[np.unique(ii).tolist() for ii in STD_TIT]
STD_ABS=[np.unique(ii).tolist() for ii in STD_ABS]
STD_CON=[np.unique(ii).tolist() for ii in STD_CON]

df2["StudiedPlace"]=[np.unique(STD_TIT[ii]+STD_ABS[ii]).tolist() for ii in range(len(STD_TIT))]

NON=[n for n,i in enumerate(df2.StudiedPlace) if len(i)==0]

len(NON)

df2["StudiedPlace"].iloc[NON]=[STD_CON[i] for i in NON]

NON=[n for n,i in enumerate(df2.StudiedPlace) if len(i)==0]

len(NON)

[jj for ii in list(df2['StudiedPlace']) for jj in ii if type(jj)!=str]

# Manually changing the missing ones 
# CSV with cities names
def CheckCities(W,CITIES):
    IND=np.where([len(jj)>0 for jj in 
        [re.findall(r'\b' + W + r'\b',str(jj)) for jj in list(CITIES.name)]])
    return(CITIES.iloc[IND])

DICT_PLC={"usa":"united states",
          "us":"united states",
          "san francisco bay":"united states",
          "hawai i.":"united states",
          "east naples":"italy",
          "himalayas":"nepal",
          "u. s.":"united states",
          "uk":"united kingdom",
          "england":"united kingdom",
          "london":"united kingdom",
          "alaska":"united states",
          "melbourne":"australia",
          "mexico city": "mexico",
          "m xico": "mexico",
          "lima": "peru",
          "paris": "france"}

CHECK=[[] for ii in range(len(STD_TIT))]

count=0
for x in df2["StudiedPlace"]:
    for ii in x:
        if ii in list(CITIES.country):
            CHECK[count].append(ii)
        elif FN.FindRoot(ii,DICT_PLC.keys(),merer=False)!="":
            CHECK[count].append(FN.FindRoot(ii,DICT_PLC.keys(),merer=False))
        else:
            SUB=CheckCities(ii,CITIES)
            SUB=Counter(SUB.country).most_common(1)
            if len(SUB)>0:
                CHECK[count].append(SUB[0][0])
            else:
                CHECK[count].append(ii)
    count+=1

[jj for ii in CHECK for jj in ii if type(jj)!=str]

df2['StudiedPlace']=CHECK

#function to convert to alpah2 country codes and continents
from pycountry_convert import country_alpha2_to_continent_code, country_name_to_country_alpha2
def get_continent(col):
    if pd.isna(col):
        return ('Unknown', 'Unknown')
    
    try:
        cn_a2_code =  country_name_to_country_alpha2(col)
    except:
        cn_a2_code = 'Unknown' 
    try:
        cn_continent = country_alpha2_to_continent_code(cn_a2_code)
    except:
        cn_continent = 'Unknown' 
    return (cn_a2_code, cn_continent)

df2['affiliation_Country'].apply(lambda x: [] if type(x)!=list else x)

df2['affiliation_ISO2']=df2['affiliation_Country'].apply(lambda x:\
            np.unique([(get_continent(ii))[0] for ii in x if (get_continent(ii.title()))[0]!="Unknown"]))
df2['affiliation_Continent']=df2['affiliation_Country'].apply(lambda x:\
            np.unique([(get_continent(ii))[1] for ii in x if (get_continent(ii.title()))[1]!="Unknown"]))

df2['StudiedPlace_ISO2']=df2['StudiedPlace'].apply(lambda x:\
            np.unique([(get_continent(ii.title()))[0] for ii in x if (get_continent(ii.title()))[0]!="Unknown"]))
df2['StudiedPlace_Continent']=df2['StudiedPlace'].apply(lambda x:\
            np.unique([(get_continent(ii.title()))[1] for ii in x if (get_continent(ii.title()))[1]!="Unknown"]))

NON=[n for n,i in enumerate(df2.StudiedPlace_ISO2) if len(i)==0]

len(NON)

## Last attempt to get the country
df2['StudiedPlace'][NON]

CITIES.subcountry= CITIES.subcountry.fillna('-')

for ii in NON:
    check2=df2['StudiedPlace'].iloc[ii]
    name=[]
    for cc in check2:
        CC1=np.unique(CITIES.country.iloc[np.where(CITIES.name==cc)]) # 
        CC2=np.unique(CITIES.country.iloc[np.where(CITIES.subcountry==cc)]) # 
        CC3=np.unique(CITIES.country.iloc[np.where([len(x)>0 for x in
                                                    [re.findall(r'(?:^|\W)' + jj + r'(?:$|\W)',cc) for jj in list(CITIES.country)]])])
        CC4=np.unique(CITIES.country.iloc[np.where([len(x)>0 for x in 
                                                    [re.findall(r'(?:^|\W)' + jj + r'(?:$|\W)',cc) for jj in list(CITIES.subcountry)]])])
        if len(CC4)>2:
            CC4=np.unique([])
        name.append(CC1.tolist()+CC2.tolist()+CC3.tolist()+CC4.tolist())    
    name=np.unique([item for sublist in name for item in sublist]).tolist()
    name=[x for x in name if len(x)>3]
    if len(name)>0:
        print("Index: "+str(ii)+" "+str(check2)+" "+str(name))
        df2['StudiedPlace'].iloc[ii]=name

df2['StudiedPlace_ISO2']=df2['StudiedPlace'].apply(lambda x:\
            np.unique([(get_continent(ii.title()))[0] for ii in x if (get_continent(ii.title()))[0]!="Unknown"]))
df2['StudiedPlace_Continent']=df2['StudiedPlace'].apply(lambda x:\
            np.unique([(get_continent(ii.title()))[1] for ii in x if (get_continent(ii.title()))[1]!="Unknown"]))

NON=[n for n,i in enumerate(df2.StudiedPlace_ISO2) if len(i)==0]
len(NON)

## Checking if the region is in the title or abstract for the NONs
REGIONS=pd.read_csv("/DATA/Just_Regions.csv")

for cc in REGIONS.columns:
    REGIONS[cc]=REGIONS[cc].str.lower()

for ii in NON:
    text=df2.title.iloc[ii]+df2.abstract.iloc[ii]+df2.conclusions[ii]+df2.discussion[ii]
    CC3=np.unique(REGIONS.Sub_Region.iloc[np.where([len(x)>0 for x in
                                    [re.findall(r'(?:^|\W)' + jj + r'(?:$|\W)',text) for jj in list(REGIONS.Intermediate_Region)]])])
    if len(CC3)>0:
        CC3=CC3.tolist()
        print("Index: "+str(ii)+" "+str(CC3))
        df2['StudiedPlace'].iloc[ii]=CC3
        df2['StudiedPlace_ISO2'].iloc[ii]=CC3
        df2['StudiedPlace_Continent'].iloc[ii]=CC3

NON=[n for n,i in enumerate(df2.StudiedPlace_Continent) if len(i)==0]
len(NON)

#### Adding type of CC hazard ####
HZD_DICT=pd.read_csv("/DATA/Hazards_Dict.csv",sep = ";")

df2["HAZARD"]=""

for ii in range(df2.shape[0]):
    IND=np.where([len(jj)>0 for jj in [re.findall(jj, df2.introduction[ii]) for jj in HZD_DICT.Hazard]])
    
    if len(IND[0])==1:
        df2["HAZARD"].iloc[ii]=HZD_DICT.Hazard[IND[0][0]]
    if len(IND[0])>2:
        df2["HAZARD"].iloc[ii]='/'.join(HZD_DICT.Hazard[IND[0]])

sum(df2["HAZARD"]!="")
         
#### Type of Study: Qualy vs Quanty ####
STD_DICT=pd.read_csv("/DATA/Qualitative.csv",sep = ";")
QUAL=STD_DICT[STD_DICT.Type=="qualitative"]
QUAL=QUAL.reset_index(drop=True)
QUAT=STD_DICT[STD_DICT.Type=="quantitative"]
QUAT=QUAT.reset_index(drop=True)

df2["qualitative"]=""
for ii in range(df2.shape[0]):
    IND=np.where([len(jj)>0 for jj in [re.findall(jj, df2.abstract[ii]+df2.methodology[ii]) for jj in QUAL.Vocabulary]])
    
    if len(IND[0])==1:
        df2["qualitative"].iloc[ii]=QUAL.Vocabulary[IND[0][0]]
    if len(IND[0])>2:
        df2["qualitative"].iloc[ii]='/'.join(QUAL.Vocabulary[IND[0]])

df2["quantitative"]=""
for ii in range(df2.shape[0]):
    IND=np.where([len(jj)>0 for jj in [re.findall(jj, df2.abstract[ii]+df2.methodology[ii]) for jj in QUAT.Vocabulary]])
    
    if len(IND[0])==1:
        df2["quantitative"].iloc[ii]=QUAT.Vocabulary[IND[0][0]]
    if len(IND[0])>2:
        df2["quantitative"].iloc[ii]='/'.join(QUAT.Vocabulary[IND[0]])

df2["both_type"]=df2.apply(lambda x: 1 if x.qualitative!="" and x.quantitative!="" else 0,axis=1)

sum(df2["both_type"])

# with open('/PROCESSED/df2.pickle', 'wb') as handle:
#     pickle.dump(df2, handle, protocol=pickle.HIGHEST_PROTOCOL)

# with open('/PROCESSED/df2.pickle', 'rb') as handle:
#     df2 = pickle.load(handle)

#### Checking Farm[a-z]* ####
df2.text2=df2.apply(lambda x: x.title+x.abstract+x.conclusions+x.discussion,axis=1)

DT1=df2[df2.apply(lambda x: x.TXT.find("farmer")>-1 or x.TXT.find("agriculture")>-1,axis=1)].copy()

DT1=DT1.reset_index(drop=True)

HZD=[ii.split("/") for ii in DT1["HAZARD"]]
HZD=Counter([item for sublist in HZD for item in sublist])

plt.bar(HZD.keys(), HZD.values())

DT1["HAZARD"].value_counts().plot(kind='barh').invert_yaxis()
sum(DT1["HAZARD"]!="")/DT1.shape[0]
sum(DT1["both_type"]!="/")/DT1.shape[0]
np.where(DT1["both_type"]=="/")

## Farmers adaption strategies
ADAPT=pd.read_csv("DATA/Farmers_AdaptationOptions2_python.csv",sep = ",")

DT1["ADAPT"]=""
DT1["ADAPT_RGX"]=""

for ii in range(DT1.shape[0]):
    IND=np.where([len(jj)>0 for jj in [re.findall(" "+kk+"(\s|.|)",\
                DT1.abstract[ii]+DT1.findings.iloc[ii]+
                DT1.conclusions.iloc[ii]+DT1.discussion.iloc[ii])\
                for kk in ADAPT.Adaptation_Strategy_RGX]])
    if len(IND[0])==1:
        DT1["ADAPT"].iloc[ii]=ADAPT.Adaptation_Strategy[IND[0][0]]
        DT1["ADAPT_RGX"].iloc[ii]=ADAPT.Adaptation_Strategy_RGX[IND[0][0]]
    if len(IND[0])>2:
        DT1["ADAPT"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy[IND[0]])
        DT1["ADAPT_RGX"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy_RGX[IND[0]])
        
sum(DT1["ADAPT"]!="")

ADP=[ii.split("/") for ii in DT1["ADAPT"]]
ADP=Counter([item for sublist in ADP for item in sublist])
ADP.most_common()

len(np.where(DT1["ADAPT"]=='')[0])
sum(DT1.apply(lambda x: len(x.StudiedPlace_ISO2)==0,axis=1))
sum(DT1.apply(lambda x: len(x.StudiedPlace_Continent)==0,axis=1))

# =============================================================================
# Performing the same steps but saving by article
# =============================================================================

DT1["factors"]=[list() for x in range(DT1.shape[0])]

for ii in range(DT1.shape[0]):
    print(str(ii))
    IND=DT1["ADAPT_RGX"].iloc[ii].split("/")
    if len(IND)>1:
        N=len(IND)
        for jj0 in range(N):
            a=IND[jj0]

            check=[x.group() for x in re.finditer(\
                "([a-z\s]*(|,|:|;|))*(?="+a+")([a-z\s]*(|,|:|;|))*(?<=.)",\
                DT1.findings.iloc[ii]+" "+
                DT1.conclusions.iloc[ii]+" "+DT1.discussion.iloc[ii])]
                
            if len(check)>0:
                DT1.factors.iloc[ii]+=check
    
    DT1.factors.iloc[ii]=[str(ss) for ss in np.unique(DT1.factors.iloc[ii])]

# with open('PROCESSED/DT1.pickle', 'wb') as handle:
#     pickle.dump(DT1, handle, protocol=pickle.HIGHEST_PROTOCOL)

# DT1.to_csv("PROCESSED\\DT1.csv",index=False)

