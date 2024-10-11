# -*- coding: utf-8 -*-
"""
Created on Fri Mar 17 08:54:24 2023

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
import spacy as spacy
# Next is to tokenize
from spacy.tokenizer import Tokenizer
from spacy.util import compile_prefix_regex,compile_infix_regex, compile_suffix_regex
# Next is for the pipeline
nlp = spacy.load('en_core_web_trf') # Small English language model

# Plots 
# from wordcloud import WordCloud
from matplotlib import pyplot as plt
from matplotlib import patches
# import seaborn as sns
import textwrap as twr

# =============================================================================
# Functions
# =============================================================================
os.chdir("C:\\Dropbox\\TU_Delft\\Projects\\ML_FindingsGrammar\\CODE\\Processing_PDFs\\")

import Functions as FN
import DataViz as DV
# import Text_2_Net as NT2
import FindTextPatterns as PTN

# =============================================================================
# Openning the data
# =============================================================================

with open('C:/Dropbox/TU_Delft/Projects/Farmers_CCA/PROCESSED/STS.pickle', 'rb') as handle:
    STS = pickle.load(handle)

with open('C:/Dropbox/TU_Delft/Projects/Farmers_CCA/PROCESSED/DT1.pickle', 'rb') as handle:
    DT1 = pickle.load(handle)

STS_un={}
for k,v in STS.items():
    STS_un[k]=[str(ss) for ss in np.unique(v)]

# =============================================================================
# Transforming verb into sign: First create dictionary
# =============================================================================
VERBS_dict=PTN.SignDict()


# =============================================================================
# Extract all NOUNS related to the verbs
# =============================================================================

DT1["text2"]=DT1.apply(lambda x: x.abstract+x.conclusions+x.discussion,axis=1)

text=DT1["text2"].iloc[2]
              
def check_verb(vv, text):
    return([x.group() for x in re.finditer(\
            "([a-z\s]*(|,|:|;|))*(?="+vv+")([a-z\s]*(|,|:|;|))*(?<=.)",text)])

VERBS=list(np.unique(VERBS_dict.index))

#### Looking for the Verbs ####

DT1_FACTORS=pd.DataFrame(columns=["ID_ART","doi",
                                  "affiliation_Continent","StudiedPlace_Continent",
                                  "affiliation_ISO2","StudiedPlace_ISO2","STR_ORG",
                                  "ADAPT","Verb_lm","NOUNA","NOUNB","VERB"])


for jj in tqdm(range(DT1.shape[0])):
    # break
    text=DT1["text2"].iloc[jj]
    sentences=re.split('(?<=[\.\?\!])\s', text) # Split sentences
    # Extracting the verbs
    text2=[(token.lemma_,str(token),text) for text in sentences\
           for token in nlp(text)  if token.pos_=="VERB"]
    # Checking the verbs that are in VERBS
    IND=[tt for tt in text2 if tt[0] in VERBS]
    
    docs = nlp.pipe([dd[2] for dd in IND])
    for j, doc in enumerate(docs):
        # break
        try:
            STR=IND[j][0]
            for values in PTN.PresentSentence(doc, STR): # PTN.extract_rel_verbs(doc, STR):
                # break
                if len(values)>0:
                    DT1_FACTORS=pd.concat([DT1_FACTORS,
                                   pd.DataFrame({
                                       "ID_ART":jj,\
                                       "doi":DT1["doi"].iloc[jj],\
                                       "affiliation_Continent":str(DT1["affiliation_Continent"].iloc[jj]),\
                                       "StudiedPlace_Continent":str(DT1["StudiedPlace_Continent"].iloc[jj]),\
                                       "affiliation_ISO2":str(DT1["affiliation_ISO2"].iloc[jj]),\
                                       "StudiedPlace_ISO2":str(DT1["StudiedPlace_ISO2"].iloc[jj]),\
                                       "STR_ORG":str(doc),\
                                       "ADAPT":DT1["ADAPT"].iloc[jj],\
                                       "Verb_lm":IND[j][0],\
                                       "NOUNA":values[0].strip(),\
                                       "NOUNB":values[2].strip(),\
                                       "VERB":values[1]}, index=[0])],\
                        ignore_index=True)
        except:
            print(str(jj) +" " +str(j))


# Tracking consistency of database
sum(DT1.apply(lambda x: len(x.StudiedPlace_Continent)>0,axis=1))
len(np.unique(DT1_FACTORS[DT1_FACTORS.apply(lambda x: len(x.StudiedPlace_Continent)>0,axis=1)].ID_ART))

DT1_FACTORS_org=DT1_FACTORS.copy()

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS['NOUNB']!='']
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS['NOUNA']!='']
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

A=DT1.index
B=np.unique(DT1_FACTORS.ID_ART)
[x for x in A if x not in B]


# =============================================================================
# Splitting text in commas
# =============================================================================

TEMPORAL=pd.DataFrame(columns=["ID_ART","doi",
                                  "affiliation_Continent","StudiedPlace_Continent",
                                  "affiliation_ISO2","StudiedPlace_ISO2","STR_ORG",
                                  "ADAPT","Verb_lm","NOUNA","NOUNB","VERB"])

for ss in tqdm(range(DT1_FACTORS.shape[0])):
    NOUNA=re.split(',|;|\Wand\W',DT1_FACTORS.loc[ss]["NOUNA"])
    NOUNB=re.split(',|;|\Wand\W',DT1_FACTORS.loc[ss]["NOUNB"])
    for ii in NOUNA:
        for jj in NOUNB:
            TEMPORAL=pd.concat([TEMPORAL,pd.DataFrame({"ID_ART":DT1_FACTORS["ID_ART"].iloc[ss],\
                                    "doi":DT1_FACTORS["doi"].iloc[ss],\
                                    "affiliation_Continent":DT1_FACTORS["affiliation_Continent"].iloc[ss],\
                                    "StudiedPlace_Continent":DT1_FACTORS["StudiedPlace_Continent"].iloc[ss],\
                                    "affiliation_ISO2":DT1_FACTORS["affiliation_ISO2"].iloc[ss],\
                                    "StudiedPlace_ISO2":DT1_FACTORS["StudiedPlace_ISO2"].iloc[ss],\
                                    "STR_ORG":DT1_FACTORS["STR_ORG"].iloc[ss],\
                                    "ADAPT":DT1_FACTORS["ADAPT"].iloc[ss],\
                                    "Verb_lm":DT1_FACTORS["Verb_lm"].iloc[ss],\
                                    "NOUNA":ii.strip(),\
                                    "NOUNB":jj.strip(),\
                                    "VERB":DT1_FACTORS["VERB"].iloc[ss]}, index=[0])],\
                               ignore_index=True)

TEMPORAL=TEMPORAL[list(TEMPORAL.apply(lambda x: x.NOUNA!="" and x.NOUNB!="", axis=1))]
TEMPORAL=TEMPORAL[list(TEMPORAL.apply(lambda x: len(x.NOUNA)>2 and len(x.NOUNB)>2, axis=1))]

TEMPORAL=TEMPORAL[list(TEMPORAL.apply(lambda x: not x.NOUNA==x.NOUNB, axis=1))]
TEMPORAL=TEMPORAL.reset_index(drop=True)

DT1_FACTORS=TEMPORAL.copy()


# =============================================================================
# Adding the sign 
# =============================================================================

DT1_FACTORS["SIGN"]=""
n=DT1_FACTORS.shape[0]
RMV=[]

for ii in range(n):
    if len(VERBS_dict.loc[DT1_FACTORS.VERB.iloc[ii]])>2:
        DT1_FACTORS.SIGN.iloc[ii]=VERBS_dict.loc[DT1_FACTORS.VERB.iloc[ii]]["SIGN_"]
    else:
        dtn=DT1_FACTORS.NOUNB.iloc[ii]
        DIRECT=VERBS_dict.loc[DT1_FACTORS.VERB.iloc[ii]]
        drt_any=0
        for dd in range(2):
            srh=DIRECT["NOTE"].iloc[dd]
            if re.search(srh,dtn)!=None:
                DT1_FACTORS.SIGN.iloc[ii]=DIRECT["SIGN_"].iloc[dd]
                DT1_FACTORS.NOUNB.iloc[ii]=re.sub(srh+" [a-z]*(\s|)","",dtn)
                drt_any=1
                break
        if drt_any==0: # If there is no direction then drop row
            RMV.append(ii)


DT1_FACTORS=DT1_FACTORS.drop(index=RMV)
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS['NOUNB']!='']
DT1_FACTORS=DT1_FACTORS[DT1_FACTORS['NOUNB']!=' ']
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

DT1_FACTORS=DT1_FACTORS[list(DT1_FACTORS.apply(lambda x: not x.NOUNA==x.NOUNB, axis=1))]
DT1_FACTORS=DT1_FACTORS[list(DT1_FACTORS.apply(lambda x: not (len(x.NOUNA)<2 or len(x.NOUNB)<2), axis=1))]
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.NOUNA.find("gender")>-1,axis=1)]
DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.VERB.find("provide")>-1 and x.NOUNB.find("technology")>-1,axis=1)]

DT1_FACTORS["source"]=DT1_FACTORS.NOUNA
DT1_FACTORS["target"]=DT1_FACTORS.NOUNB

# =============================================================================
# Cleaning Factors Using the Adaptation Strategies
# =============================================================================
## Farmers adaption strategies
ADAPT=pd.read_csv("C:/Dropbox/TU_Delft/Projects/Farmers_CCA/DATA/Farmers_AdaptationOptions2_python.csv",sep = ",")

DT1_FACTORS["ADAPT_S"]=""
DT1_FACTORS["ADAPT_RGX_S"]=""

DT1_FACTORS["ADAPT_T"]=""
DT1_FACTORS["ADAPT_RGX_T"]=""

for ii in tqdm(range(DT1_FACTORS.shape[0])):
    
    IND_S=np.where([ len(x)>0 for x in list(map(lambda x: re.findall(x,DT1_FACTORS.NOUNA[ii]),
             ADAPT.Adaptation_Strategy_RGX))])
    
    ## Adding those that have adaptation as verb
    if DT1_FACTORS.VERB[ii]=="adapt":
        DT1_FACTORS.NOUNB[ii]="adapt "+DT1_FACTORS.NOUNB[ii]
        
    IND_T=np.where([ len(x)>0 for x in list(map(lambda x: re.findall(x,DT1_FACTORS.NOUNB[ii]),
             ADAPT.Adaptation_Strategy_RGX))])
    
    if len(IND_S[0])>=1:
        DT1_FACTORS["ADAPT_S"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy[IND_S[0]])
        DT1_FACTORS["ADAPT_RGX_S"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy_RGX[IND_S[0]])
    
    if len(IND_T[0])>=1:
        DT1_FACTORS["ADAPT_T"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy[IND_T[0]])
        DT1_FACTORS["ADAPT_RGX_T"].iloc[ii]='/'.join(ADAPT.Adaptation_Strategy_RGX[IND_T[0]])



DT1_FACTORS["source"]=DT1_FACTORS.apply(lambda x: x.ADAPT_S 
                                          if x.ADAPT_S!='' else x.NOUNA, axis=1)
DT1_FACTORS["target"]=DT1_FACTORS.apply(lambda x: x.ADAPT_T 
                                          if x.ADAPT_T!='' else x.NOUNB, axis=1)

## Dealing with joinned adaptations "/"
DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_S!='' and x.ADAPT_T!='',axis=1)].\
    drop(columns=['SIGN','source','target','ADAPT_RGX_S','ADAPT_RGX_T']).\
    groupby(['ADAPT_S','ADAPT_T']).count().\
        sort_values(['ADAPT_T'], ascending=False)

TEMPORAL=pd.DataFrame(columns=["ID_ART","doi",
                                  "affiliation_Continent","StudiedPlace_Continent",
                                  "affiliation_ISO2","StudiedPlace_ISO2","STR_ORG",
                                  "ADAPT","Verb_lm","NOUNA","VERB","NOUNB", 
                                  'SIGN', 'ADAPT_S', 'ADAPT_RGX_S',
                                  'ADAPT_T', 'ADAPT_RGX_T', 'source', 'target'])

for ss in range(DT1_FACTORS.shape[0]):
    
    NOUNA=re.split('/',DT1_FACTORS.loc[ss]["source"])
    NOUNB=re.split('/',DT1_FACTORS.loc[ss]["target"])
    # break
    NOUNA2=re.split('/',DT1_FACTORS.loc[ss]["ADAPT_S"])
    NOUNB2=re.split('/',DT1_FACTORS.loc[ss]["ADAPT_T"])
    
    NOUNA3=re.split('/',DT1_FACTORS.loc[ss]["ADAPT_RGX_S"])
    NOUNB3=re.split('/',DT1_FACTORS.loc[ss]["ADAPT_RGX_T"])
    
    N1=len(NOUNA)
    N2=len(NOUNB)
    for ii in range(N1):
        for jj in range(N2):
            # break
            TEMPORAL=pd.concat([TEMPORAL,pd.DataFrame({
                'ID_ART':DT1_FACTORS["ID_ART"].iloc[ss],\
                "doi":DT1_FACTORS["doi"].iloc[ss],\
                "affiliation_Continent":DT1_FACTORS["affiliation_Continent"].iloc[ss],\
                "StudiedPlace_Continent":DT1_FACTORS["StudiedPlace_Continent"].iloc[ss],\
                "affiliation_ISO2":DT1_FACTORS["affiliation_ISO2"].iloc[ss],\
                "StudiedPlace_ISO2":DT1_FACTORS["StudiedPlace_ISO2"].iloc[ss],\
                "STR_ORG":DT1_FACTORS["STR_ORG"].iloc[ss],\
                "ADAPT":DT1_FACTORS["ADAPT"].iloc[ss],\
                "Verb_lm":DT1_FACTORS["Verb_lm"].iloc[ss],\
                "NOUNA":DT1_FACTORS["NOUNA"].iloc[ss].strip(),\
                "VERB":DT1_FACTORS["VERB"].iloc[ss].strip(),\
                "NOUNB":DT1_FACTORS["NOUNB"].iloc[ss].strip(),
                'SIGN':DT1_FACTORS["SIGN"].iloc[ss].strip(), 
                'ADAPT_S':NOUNA2[ii].strip(), 
                'ADAPT_RGX_S':NOUNA3[ii].strip(),
                'ADAPT_T':NOUNB2[jj].strip(), 
                'ADAPT_RGX_T':NOUNB3[jj].strip(), 
                'source':NOUNA[ii].strip(), 
                'target':NOUNB[jj].strip()}, index=[0])],\
             ignore_index=True)

TEMPORAL[TEMPORAL.apply(lambda x: x.ADAPT_S!='' and x.ADAPT_T!='',axis=1)].\
    drop(columns=['SIGN','ADAPT_S','ADAPT_T','ADAPT_RGX_S','ADAPT_RGX_T']).\
    groupby(['source','target']).count().\
        sort_values(['target'], ascending=False)

DT1_FACTORS=TEMPORAL.copy()

# Replacing the Adaptation strategies in the main text:
DT1_FACTORS["source"]=DT1_FACTORS.apply(lambda x: (re.search(x.ADAPT_RGX_S,x.NOUNA))[0] 
                                          if re.search(x.ADAPT_RGX_S,x.NOUNA)!=None and x.ADAPT_RGX_S!=''\
                                              else x.NOUNA, axis=1)
DT1_FACTORS["target"]=DT1_FACTORS.apply(lambda x: (re.search(x.ADAPT_RGX_T,x.NOUNB))[0]
                                          if re.search(x.ADAPT_RGX_T,x.NOUNB)!=None and x.ADAPT_RGX_T!=''\
                                              else x.NOUNB, axis=1)

DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_T!="",axis=1)][['ID_ART','ADAPT_T', 'ADAPT_RGX_T', 'source', 'target']]
np.unique(DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_T!="",axis=1)][['ID_ART']])

# =============================================================================
# How to extract meaningful relations?
# Check if sentences are the same based on permutations 
# =============================================================================

DT1_FACTORS_org=DT1_FACTORS.copy()

#### Cleaning the sentences
DT1_FACTORS.source=DT1_FACTORS.apply(lambda x: (''.join(re.findall("[A-Za-z\s\-]",x.source))).lower(),axis=1)
DT1_FACTORS.target=DT1_FACTORS.apply(lambda x: (''.join(re.findall("[A-Za-z\s\-]",x.target))).lower(),axis=1)

# Extract the nouns that are more commonly used
NOUNA=DT1_FACTORS.copy()
NOUNA["length"]=NOUNA.apply(lambda x: len(x.source), axis=1)
NOUNA=NOUNA.groupby(["ID_ART","source","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNA=NOUNA.groupby(["source","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNA=NOUNA[NOUNA["length"]>2]
NOUNA=NOUNA[NOUNA.apply(lambda x: len(x.source.split())<6,axis=1)]
NOUNA=NOUNA.rename(columns={"source":"NOUN","ID_ART":"CountA"})
NOUNA=NOUNA[["NOUN","length","CountA"]]
NOUNA=NOUNA.reset_index(drop=True)
# NOUNA=NOUNA[NOUNA.CountA>1]

NOUNB=DT1_FACTORS.copy()
NOUNB["length"]=NOUNB.apply(lambda x: len(x.target), axis=1)
NOUNB=NOUNB.groupby(["ID_ART","target","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNB=NOUNB.groupby(["target","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNB=NOUNB[NOUNB["length"]>2]
NOUNB=NOUNB[NOUNB.apply(lambda x: len(x.target.split())<6,axis=1)]
NOUNB=NOUNB.rename(columns={"target":"NOUN","ID_ART":"CountB"})
NOUNB=NOUNB[["NOUN","length","CountB"]]
NOUNB=NOUNB.reset_index(drop=True)
# NOUNB=NOUNB[NOUNB.CountB>1]

NOUN=pd.merge(NOUNA, NOUNB, on=["NOUN","length"],how="outer")
NOUN["Count"]=NOUN.CountA.fillna(0) + NOUN.CountB.fillna(0)
# Biasing towards knowing adaptations and factors
NOUN=NOUN.fillna("")
NOUN=NOUN[NOUN.apply(lambda x: len(x.NOUN.split())>1 and len(x.NOUN.split())<=4,axis=1)]
NOUN=NOUN.sort_values(['Count'], ascending=False).reset_index(drop=True)
NOUN["NOUN_org"]=NOUN.NOUN.copy()


# =============================================================================
# Check if sentences are the same based on permutations 
# =============================================================================
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

from itertools import permutations

LOOP=set(DT1_FACTORS.index)

N1=NOUN.shape[0]
for i1 in tqdm(range(N1)):
    a=NOUN.NOUN.iloc[i1].split()
    p=[' '.join(ii) for ii in permutations(a)]
    pn=[(list(LOOP)[c],x) for c,x in enumerate(DT1_FACTORS.iloc[list(LOOP)].source) if x in p]
    if len(pn)>0:
        # break
        a=" ".join(a)
        for j in pn:
            LOOP.discard(j[0])
            if j!=a:
                DT1_FACTORS.source.iloc[j[0]]=a
                
                
LOOP=set(DT1_FACTORS.index)
N1=NOUN.shape[0]
for i1 in tqdm(range(N1)):
    a=NOUN.NOUN.iloc[i1].split()
    p=[' '.join(ii) for ii in permutations(a)]
    pn=[(list(LOOP)[c],x) for c,x in enumerate(DT1_FACTORS.iloc[list(LOOP)].target) if x in p]
    if len(pn)>0:
        # break
        a=" ".join(a)
        for j in pn:
            LOOP.discard(j[0])
            if j!=a:
                DT1_FACTORS.target.iloc[j[0]]=a

# Replace them in the database depending on length
dt_word,voc=FN.CorrMtx(DT1,column="text2")
PROBAS=[FN.ProbaWords(W1,dt_word,voc) for W1 in voc]
MED=np.quantile(PROBAS,0.5)  #FN.ProbaWords("farm",dt_word,voc)
DT1_FACTORS["NOUNA_sht1"]=DT1_FACTORS.source.progress_apply(lambda x: FN.FindRootCorr2(x,dt_word,voc,p=0.40,PR_Ws=PROBAS,MED=MED))
DT1_FACTORS["NOUNB_sht1"]=DT1_FACTORS.target.progress_apply(lambda x: FN.FindRootCorr2(x,dt_word,voc,p=0.40,PR_Ws=PROBAS,MED=MED))

A=Counter(DT1_FACTORS["NOUNA_sht1"])
(A).most_common(20)
B=Counter(DT1_FACTORS["NOUNB_sht1"])
(B).most_common(20)

DT1_FACTORS=DT1_FACTORS[list(DT1_FACTORS.apply(lambda x: len(x.NOUNA_sht1)>2 and len(x.NOUNB_sht1)>2, axis=1))]
DT1_FACTORS=DT1_FACTORS[list(DT1_FACTORS.apply(lambda x: not x.NOUNA_sht1==x.NOUNB_sht1, axis=1))]
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

# =============================================================================
# How to extract meaningful relations?
# Check if sentences are the same based on permutations 
# =============================================================================

# Extract the nouns that are more commonly used
NOUNA=DT1_FACTORS.copy()
NOUNA["length"]=NOUNA.apply(lambda x: len(x.NOUNA_sht1), axis=1)
NOUNA=NOUNA.groupby(["ID_ART","NOUNA_sht1","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNA=NOUNA.groupby(["NOUNA_sht1","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNA=NOUNA[NOUNA["length"]>2]
NOUNA=NOUNA[NOUNA.apply(lambda x: len(x.NOUNA_sht1.split())<6,axis=1)]
NOUNA=NOUNA.rename(columns={"NOUNA_sht1":"NOUN","ID_ART":"CountA"})
NOUNA=NOUNA[["NOUN","length","CountA"]]
NOUNA=NOUNA.reset_index(drop=True)
# NOUNA=NOUNA[NOUNA.CountA>1]

NOUNB=DT1_FACTORS.copy()
NOUNB["length"]=NOUNB.apply(lambda x: len(x.NOUNB_sht1), axis=1)
NOUNB=NOUNB.groupby(["ID_ART","NOUNB_sht1","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNB=NOUNB.groupby(["NOUNB_sht1","length"],as_index=False).count().sort_values(['VERB'], ascending=False)
NOUNB=NOUNB[NOUNB["length"]>2]
NOUNB=NOUNB[NOUNB.apply(lambda x: len(x.NOUNB_sht1.split())<6,axis=1)]
NOUNB=NOUNB.rename(columns={"NOUNB_sht1":"NOUN","ID_ART":"CountB"})
NOUNB=NOUNB[["NOUN","length","CountB"]]
NOUNB=NOUNB.reset_index(drop=True)
# NOUNB=NOUNB[NOUNB.CountB>1]

NOUN=pd.merge(NOUNA, NOUNB, on=["NOUN","length"],how="outer")
NOUN["Count"]=NOUN.CountA.fillna(0) + NOUN.CountB.fillna(0)
# Biasing towards knowing adaptations and factors
NOUN=NOUN.fillna("")
NOUN=NOUN[NOUN.apply(lambda x: len(x.NOUN.split())>1 and len(x.NOUN.split())<=4,axis=1)]
NOUN=NOUN.sort_values(['Count'], ascending=False).reset_index(drop=True)
NOUN["NOUN_org"]=NOUN.NOUN.copy()


# =============================================================================
# Check if sentences are the same based on permutations 
# =============================================================================
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

from itertools import permutations

LOOP=set(DT1_FACTORS.index)

N1=NOUN.shape[0]
for i1 in tqdm(range(N1)):
    a=NOUN.NOUN.iloc[i1].split()
    p=[' '.join(ii) for ii in permutations(a)]
    pn=[(list(LOOP)[c],x) for c,x in enumerate(DT1_FACTORS.iloc[list(LOOP)].NOUNA_sht1) if x in p]
    if len(pn)>0:
        # break
        a=" ".join(a)
        for j in pn:
            # break
            LOOP.discard(j[0])
            if j!=a:
                DT1_FACTORS.NOUNA_sht1.iloc[j[0]]=a
                
                
LOOP=set(DT1_FACTORS.index)
N1=NOUN.shape[0]
for i1 in tqdm(range(N1)):
    a=NOUN.NOUN.iloc[i1].split()
    p=[' '.join(ii) for ii in permutations(a)]
    pn=[(list(LOOP)[c],x) for c,x in enumerate(DT1_FACTORS.iloc[list(LOOP)].NOUNB_sht1) if x in p]
    if len(pn)>0:
        # break
        a=" ".join(a)
        for j in pn:
            LOOP.discard(j[0])
            if j!=a:
                DT1_FACTORS.NOUNB_sht1.iloc[j[0]]=a

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS.apply(lambda x: len(x.NOUNA_sht1)>1 and len(x.NOUNB_sht1)>1,axis=1)]


# =============================================================================
# Checking which ones are Adaptation Measures
# =============================================================================

DT1_FACTORS2=DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_S!="" or x.ADAPT_T!="",axis=1)]

DT1_FACTORS2["source1"]=DT1_FACTORS2.NOUNA_sht1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
DT1_FACTORS2["target1"]=DT1_FACTORS2.NOUNB_sht1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

# Creating Database with frequencies
FACTORS=DT1_FACTORS2[['Verb_lm','ID_ART','source1', 'target1','SIGN']]
FACTORS["VALUE"]=1.0
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.ADAPT_S!="" and x.ADAPT_T!="",axis=1)]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS['N_T']=len(np.unique(FACTORS.ID_ART))
N_ID_ART=Counter(FACTORS["ID_ART"])
FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1=="social\n capital" and x.target1=="climate\n change\n adaptation",axis=1)]
# Proportion is relative to the ID_ART
FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_T','N_ID_ART'],
                        as_index=False)["VALUE"].sum()
FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
FACTORS=FACTORS.drop(columns=['ID_ART','N_T','N_ID_ART'])
FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"],columns='SIGN',values='VALUE',fill_value=0)
FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS["SIGN"]=FACTORS.apply(lambda x: ["+","-","+/-"][np.argmax([x["+"],x["-"],x["+/-"]])],axis=1)
FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN],axis=1)/FACTORS["ONE"]
FACTORS=FACTORS.drop(columns=['+','-','+/-'])
FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                "ONE":"degree",
                                "source1":"source",
                                "target1":"target"})


# FACTORS.to_csv("C:/Dropbox/TU_Delft/Projects/Farmers_CCA/PROCESSED/FACTORS2.csv", float_format = '{:.2%}'.format)

W="social\n capital"
DB=FACTORS[["source","target","SIGN","edge_weight","degree"]][FACTORS.apply(lambda x: not pd.isnull(re.search(W,x.source+x.target)),axis=1)]
DV.PlotText_ACC(DB,fontsize=11,delta_edge=0.5,min_target_margin=20) # ,CENTER=True
W2=W.replace('\n','')
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/{W2}_2.png",dpi=300)

G,pos=DV.PosModularity(DB,dw=100,delta=100,delta_edge=0.05,resolution=1)
DV.plotNet(G,pos,fontsize=8,VERTEX=True,MODULARITY=True,N_M=8,loc_legend='lower center') 
plt.savefig(f"C:\\Dropbox\\TU_Delft\\Projects\\Unsupervised\\Version2\\IMAGES\\{W2}_2.png",dpi=300)


W="information"
W2=W.replace('\n','')
DB=FACTORS[["source","target","SIGN","edge_weight","degree"]][FACTORS.apply(lambda x: \
                                  (x.source.startswith(W) and x.source.endswith(W))\
                                  or (x.target.startswith(W) and x.target.endswith(W)),\
                                      axis=1)]
# DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
#                                  (x.source.find(W)>-1 or x.target.find(W)>-1),axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0.01)
DV.plotNet(G,pos,fontsize=10,VERTEX=True,MODULARITY=True,N_M=4,loc_legend='lower center') # ,N_M=4
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/First10Papers_3.png",
            dpi=300, bbox_inches='tight')

W="information"
W2=W.replace('\n','')
## source
DB=FACTORS[["source","target","SIGN","edge_weight","degree"]][FACTORS.apply(lambda x: \
                                 (not x.source.startswith(W) and not x.source.endswith(W))\
                                 and (x.target.startswith(W) and x.target.endswith(W)),\
                                     axis=1)]
DV.PlotText_ACC(DB,fontsize=10,delta_edge=0.5,min_target_margin=20,CENTER=True) # ,CENTER=True
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/{W2}_target3.png",
            dpi=300, bbox_inches='tight')
## target
DB=FACTORS[["source","target","SIGN","edge_weight","degree"]][FACTORS.apply(lambda x: \
                                 (x.source.startswith(W) and x.source.endswith(W))\
                                 and (not x.target.startswith(W) and not x.target.endswith(W)),\
                                     axis=1)]
DV.PlotText_ACC(DB,fontsize=10,delta_edge=0.5,min_target_margin=20,CENTER=True) # ,CENTER=True
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/{W2}_source3.png",
            dpi=300, bbox_inches='tight')

W=('adaptat[a-z]*\n [a-z]*(\n|\s|){0,2}climate\n change',
   'adaptat[a-z]*\n [a-z]*(\n|\s|){0,2}measure',
   'adaptat[a-z]*\n [a-z]*(\n|\s|){0,2}capacity',
   'farm[a-z]*\n [a-z]*(\n|\s|){0,2}adaptat[a-z]*','adapt[a-z]*\n [a-z]*(\n|\s|){0,2}strategy',
   'climate\n adaptat[a-z]*','adaptat[a-z]*\n [a-z]*(\n|\s|){0,2}measure')
DB=FACTORS[FACTORS.apply(lambda x: sum([len(re.findall(w,x.source))for w in W ])==0 and
                         sum([len(re.findall(w,x.target)) for w in W ])>0,axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0,resolution=1)
DV.plotNet(G,pos,fontsize=9,VERTEX=True,MODULARITY=True,N_M=10,loc_legend='lower center') 
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/adaptation2_2.png",
            dpi=300, bbox_inches='tight')

DB.target="climate\n change\n adaptation"
DB=DB.groupby(['source','target']).agg({
    'degree' : lambda x: x.sum(),
    'edge_weight': lambda x: x.mean(),
    'SIGN':lambda x: Counter(x).most_common(1)[0][0]})
DB=DB.reset_index()
DV.PlotText_ACC(DB,fontsize=8,delta_edge=0.5,min_target_margin=20,CENTER=True) # ,CENTER=True
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/CCA_2.png",
            dpi=1200,bbox_inches='tight')


# (50,60),(60,70),(70,80),(80,90),(85,95),(110,120),(120,130)
DB=FACTORS[["source","target","SIGN","edge_weight","degree"]].sample(frac=0.05) # 
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0.01)
DV.plotNet(G,pos,fontsize=8,VERTEX=True,MODULARITY=True,N_M=4,loc_legend='lower center') # ,N_M=4
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/First10Papers_3.png",
            dpi=300, bbox_inches='tight')


DB=FACTORS
G,pos=DV.PosModularity(DB,dw=150,delta=100,delta_edge=0)
DV.plotNet(G,pos,fontsize=2,MODULARITY=True,
           min_target_margin=3,loc_legend=[0.9,0.1],N_M=10,VERTEX=False)
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/nClusters_2.png",dpi=300)


W="climate\n change"
DB=FACTORS[FACTORS.apply(lambda x: \
                                 (x.source.find(W)>-1)or (x.target.find(W)>-1),\
                                     axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50)
DV.plotNet(G,pos,fontsize=12,VERTEX=True,MODULARITY=True,N_M=1)
W2=W.replace('\n','')
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/{W2}_2.png",dpi=1200)


W="adaptation"
DB=FACTORS[FACTORS.apply(lambda x: \
                                 (x.source.find(W)>-1)or (x.target.find(W)>-1),\
                                     axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=100)
DV.plotNet(G,pos,fontsize=5,VERTEX=False,MODULARITY=True,N_M=4)
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/{W}_2.png",dpi=1200)

# =============================================================================
# Create clusters for Adaptation Measures
# =============================================================================

FACTORS=DT1_FACTORS2[['Verb_lm','ID_ART','NOUNA_sht1','ADAPT_S','NOUNB_sht1','ADAPT_T','SIGN']]

FACTORS.source=FACTORS.apply(lambda x: x.ADAPT_S if x.ADAPT_S!='' else x.NOUNA_sht1,axis=1)
FACTORS.target=FACTORS.apply(lambda x: x.ADAPT_T if x.ADAPT_T!='' else x.NOUNB_sht1,axis=1)

FACTORS["source1"]=FACTORS.source.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
FACTORS["target1"]=FACTORS.target.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))


# Creating Database with frequencies
FACTORS=FACTORS[['Verb_lm','ID_ART','source1', 'target1','SIGN']]
FACTORS["VALUE"]=1.0
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.ADAPT_S!="" and x.ADAPT_T!="",axis=1)]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS['N_T']=len(np.unique(FACTORS.ID_ART))
N_ID_ART=Counter(FACTORS["ID_ART"])
FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1=="social\n capital" and x.target1=="climate\n change\n adaptation",axis=1)]
# Proportion is relative to the ID_ART
FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_T','N_ID_ART'],
                        as_index=False)["VALUE"].sum()
FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
FACTORS=FACTORS.drop(columns=['ID_ART','N_T','N_ID_ART'])
FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"],columns='SIGN',values='VALUE',fill_value=0)
FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS["SIGN"]=FACTORS.apply(lambda x: ["+","-","+/-"][np.argmax([x["+"],x["-"],x["+/-"]])],axis=1)
FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN],axis=1)/FACTORS["ONE"]
FACTORS=FACTORS.drop(columns=['+','-','+/-'])
FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                "ONE":"degree",
                                "source1":"source",
                                "target1":"target"})

DB=FACTORS
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0.1,resolution=1)
DV.plotNet(G,pos,fontsize=3,VERTEX=False,MODULARITY=True,min_target_margin=5)
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\\IMAGES\\Check_1_20230731.png",dpi=1200)

# Second way is to create the clusters manually 
G,pos=DV.PosModularity(DB,dw=100,delta=100,weight="weight")

CCA_Measures=np.unique(DT1_FACTORS2.apply(lambda x: x.ADAPT_S if x.ADAPT_S!='' else (x.ADAPT_T if x.ADAPT_T!='' else ''),axis=1))
CCA_Measures=[DV.AddBreakLine(x,n=1,breakAfter=3) for x in CCA_Measures]
# CCA_Measures=CCA_Measures[1:]

COMMU=[]
for cc,k in enumerate(CCA_Measures):
    try:
        COMMU.append({ii for ii in nx.all_neighbors(G, k)}.union({k}))
    except:
        print(k)

COMMU.sort(key=lambda x: len(x),reverse=True)

N=len(COMMU)
COMMU_r=[ii for ii in COMMU] # reversed(COMMU)
CCA_Measures_re=reversed(CCA_Measures)
COMMU2=COMMU_r.copy()
for cc,k in enumerate(COMMU_r):
    # break
    for ii in range(cc+1,N):
        # break
        COMMU2[ii]=COMMU2[ii].difference(k)

COMMU2.sort(key=lambda x: len(x),reverse=True)
COMMU2=[ii for ii in COMMU2 if len(ii)>0]
[item for sublist in COMMU2 for item in sublist]

G,pos=DV.PosModularity(DB,COMMU=COMMU2,dw=100,delta=100)
DV.plotNet(G,pos,fontsize=3,VERTEX=False,MODULARITY=True,min_target_margin=5,N_M=20) #
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\\IMAGES\\Check_2_20230731.png",dpi=1200)

# =============================================================================
# Based on Networks, a csv with labels was made. This is used to 
# categorize factors of adaptation
# =============================================================================

## Farmers' factors for FACTation
FACT=pd.read_csv("C:/Dropbox/TU_Delft/Projects/Farmers_CCA/PROCESSED\\Factors_Dictionary_python_SGCTF.csv",sep = ",")

DT1_FACTORS["FACT_S"]=""
DT1_FACTORS["FACT_RGX_S"]=""
DT1_FACTORS["FACT_T"]=""
DT1_FACTORS["FACT_RGX_T"]=""

for ii in range(DT1_FACTORS.shape[0]):
    
    if DT1_FACTORS["FACT_S"].iloc[ii]=="":
        IND_S=np.where([len(x)>0 for x in map(lambda x: re.findall(x,DT1_FACTORS.NOUNA[ii]),
                 FACT.Factor_Strategy_RGX)])
        if len(IND_S[0])>=1:
            DT1_FACTORS["FACT_S"].iloc[ii]='/'.join(FACT.Tatiana_Factor_Strategy[IND_S[0]])
            DT1_FACTORS["FACT_RGX_S"].iloc[ii]='/'.join(FACT.Factor_Strategy_RGX[IND_S[0]])
    
    if DT1_FACTORS["FACT_T"].iloc[ii]=="":
        IND_T=np.where([len(x)>0 for x in map(lambda x: re.findall(x,DT1_FACTORS.NOUNB[ii]),
                 FACT.Factor_Strategy_RGX)])
        if len(IND_T[0])>=1:
            DT1_FACTORS["FACT_T"].iloc[ii]='/'.join(FACT.Tatiana_Factor_Strategy[IND_T[0]])
            DT1_FACTORS["FACT_RGX_T"].iloc[ii]='/'.join(FACT.Factor_Strategy_RGX[IND_T[0]])

## Checking number of articles
A=DT1.index
B=np.unique(DT1_FACTORS.ID_ART)
[x for x in A if x not in B]


DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

## Dealing with joinned adaptations "/"
DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_S!='' and x.ADAPT_T!='',axis=1)].\
    drop(columns=['SIGN','ADAPT_S','ADAPT_T','ADAPT_RGX_S','ADAPT_RGX_T',
                  'FACT_RGX_S','FACT_RGX_T']).\
    groupby(['source','target']).count().\
        sort_values(['FACT_T'], ascending=False)

TEMPORAL=pd.DataFrame(columns=["ID_ART","doi",
                                  "affiliation_Continent","StudiedPlace_Continent",
                                  "affiliation_ISO2","StudiedPlace_ISO2","STR_ORG",
                                  "ADAPT","Verb_lm","NOUNA","VERB","NOUNB", 
                                  "NOUNA_sht1","NOUNB_sht1",
                                  'SIGN', 'ADAPT_S','FACT_S', 'ADAPT_RGX_S','FACT_RGX_S', 
                                  'ADAPT_T','FACT_T', 'ADAPT_RGX_T','FACT_RGX_T', 
                                  'source', 'target'])


for ss in range(DT1_FACTORS.shape[0]):
    
    NOUNA2=re.split('/',DT1_FACTORS.loc[ss]["FACT_S"])
    NOUNB2=re.split('/',DT1_FACTORS.loc[ss]["FACT_T"])
    
    NOUNA3=re.split('/',DT1_FACTORS.loc[ss]["FACT_RGX_S"])
    NOUNB3=re.split('/',DT1_FACTORS.loc[ss]["FACT_RGX_T"])
    
    N1=len(NOUNA2)
    N2=len(NOUNB2)
    for ii in range(N1):
        for jj in range(N2):
            # break
            TEMPORAL=pd.concat([TEMPORAL,pd.DataFrame({
                'ID_ART':DT1_FACTORS["ID_ART"].iloc[ss],\
                "doi":DT1_FACTORS["doi"].iloc[ss],\
                "affiliation_Continent":DT1_FACTORS["affiliation_Continent"].iloc[ss],\
                "StudiedPlace_Continent":DT1_FACTORS["StudiedPlace_Continent"].iloc[ss],\
                "affiliation_ISO2":DT1_FACTORS["affiliation_ISO2"].iloc[ss],\
                "StudiedPlace_ISO2":DT1_FACTORS["StudiedPlace_ISO2"].iloc[ss],\
                "STR_ORG":DT1_FACTORS["STR_ORG"].iloc[ss],\
                "ADAPT":DT1_FACTORS["ADAPT"].iloc[ss],\
                "Verb_lm":DT1_FACTORS["Verb_lm"].iloc[ss],\
                "NOUNA":DT1_FACTORS["NOUNA"].iloc[ss].strip(),\
                "VERB":DT1_FACTORS["VERB"].iloc[ss].strip(),\
                "NOUNB":DT1_FACTORS["NOUNB"].iloc[ss].strip(),
                "NOUNA_sht1":DT1_FACTORS["NOUNA_sht1"].iloc[ss].strip(),\
                "NOUNB_sht1":DT1_FACTORS["NOUNB_sht1"].iloc[ss].strip(),
                'SIGN':DT1_FACTORS["SIGN"].iloc[ss].strip(), 
                'ADAPT_S':DT1_FACTORS["ADAPT_S"].iloc[ss].strip(),
                'FACT_S':NOUNA2[ii].strip(), 
                'ADAPT_RGX_S':DT1_FACTORS["ADAPT_RGX_S"].iloc[ss].strip(),
                'FACT_RGX_S':NOUNA3[ii].strip(),
                'ADAPT_T':DT1_FACTORS["ADAPT_T"].iloc[ss].strip(),
                'FACT_T':NOUNB2[jj].strip(), 
                'ADAPT_RGX_T':DT1_FACTORS["ADAPT_RGX_T"].iloc[ss].strip(),
                'FACT_RGX_T':NOUNB3[jj].strip(), 
                'source':DT1_FACTORS["source"].iloc[ss].strip(),
                'target':DT1_FACTORS["target"].iloc[ss].strip(),}, index=[0])],\
             ignore_index=True)

TEMPORAL[TEMPORAL.apply(lambda x: x.ADAPT_S!='' and x.ADAPT_T!='',axis=1)].\
    drop(columns=['SIGN','ADAPT_S','ADAPT_T','ADAPT_RGX_S','ADAPT_RGX_T',
                  'FACT_RGX_S','FACT_RGX_T']).\
    groupby(['source','target']).count().\
        sort_values(['FACT_T'], ascending=False)

DT1_FACTORS=TEMPORAL.copy()

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS.apply(lambda x: (x.ADAPT_RGX_S!="" or x.FACT_RGX_S!="") and
                                          (x.ADAPT_RGX_T!="" or x.FACT_RGX_T!=""),axis=1)]
DT1_FACTORS=DT1_FACTORS.reset_index()

# Replacing the Adaptation strategies in the main text:
DT1_FACTORS["source"]=DT1_FACTORS.apply(lambda x: re.search(x.ADAPT_RGX_S,x.NOUNA)[0] if x.ADAPT_RGX_S!="" 
                                          else (re.search(x.FACT_RGX_S,x.NOUNA)[0] if x.FACT_RGX_S!='' else ''), axis=1)
DT1_FACTORS["source"]=DT1_FACTORS.source.apply(lambda x: x.strip())

DT1_FACTORS["target"]=DT1_FACTORS.apply(lambda x: re.search(x.ADAPT_RGX_T,x.NOUNB)[0] if x.ADAPT_RGX_T!="" 
                                          else (re.search(x.FACT_RGX_T,x.NOUNB)[0] if x.FACT_RGX_T!="" else ''), axis=1)
DT1_FACTORS["target"]=DT1_FACTORS.target.apply(lambda x: x.strip())


DT1_FACTORS=DT1_FACTORS.reset_index()

# DT1_FACTORS=DT1_FACTORS.drop_duplicates()

## Checking number of articles
A=DT1.index
B=np.unique(DT1_FACTORS.ID_ART)
[x for x in A if x not in B]

# =============================================================================
# Checking which ones are Adaptation Measures
# =============================================================================

DT1_FACTORS_ORG=DT1_FACTORS.copy()

DT1_FACTORS["source_org"]=DT1_FACTORS["source"]
DT1_FACTORS["target_org"]=DT1_FACTORS["target"]

DT1_FACTORS["source"]=DT1_FACTORS.source_org.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
DT1_FACTORS["target"]=DT1_FACTORS.target_org.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

# Creating Database with frequencies
FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','doi',
                     'affiliation_Continent','StudiedPlace_Continent',
                     'affiliation_ISO2','StudiedPlace_ISO2',
                     'ADAPT','ADAPT_S', 'FACT_S','ADAPT_T', 'FACT_T','SIGN']]
FACTORS["source"]=FACTORS.apply(lambda x: x.ADAPT_S if x.ADAPT_S!="" 
                                          else (x.FACT_S if x.FACT_S!='' else ''), axis=1)
FACTORS["source"]=FACTORS.source.apply(lambda x: x.strip())

FACTORS["target"]=FACTORS.apply(lambda x: x.ADAPT_T if x.ADAPT_T!="" 
                                          else (x.FACT_T if x.FACT_T!='' else ''), axis=1)
FACTORS["target"]=FACTORS.target.apply(lambda x: x.strip())
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)

# Proportion is relative to the ID_ART

A=DT1.index
B=np.unique(FACTORS.ID_ART)
len([x for x in A if x in B])

# FACTORS.to_csv("C:/Dropbox/TU_Delft/Projects/Farmers_CCA/PROCESSED/FACTORS2_notUsed.csv", float_format = '{:.2%}'.format)

'''
The following code is to visualize the connections
'''
DT1_FACTORS=DT1_FACTORS_ORG.copy()

DT1_FACTORS=DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.ADAPT_S!="" and x.ADAPT_T!="",axis=1)]

DT1_FACTORS["source"]=DT1_FACTORS.source.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
DT1_FACTORS["target"]=DT1_FACTORS.target.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

# Creating Database with frequencies
# FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','source_org', 'target_org','SIGN']]
FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','doi','ADAPT','source', 'target','SIGN']]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
# Proportion is relative to the ID_ART

A=DT1.index
B=np.unique(FACTORS.ID_ART)
len([x for x in A if x in B])


FACTORS=FACTORS.groupby(['ID_ART'],as_index=False).value_counts(sort=False, normalize=True)
FACTORS=FACTORS.groupby(['ID_ART','source', 'target','SIGN', 'proportion'],as_index=False).count()
FACTORS=FACTORS.rename(columns={"Verb_lm":"freq"})

# Adding weights
FACTORS["proportion"]=FACTORS["proportion"]*FACTORS["freq"]



W="social\n capital"
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
                                 (x.source.startswith(W) and x.source.endswith(W))\
                                 or (x.target.startswith(W) and x.target.endswith(W)),\
                                     axis=1)]
DV.PlotText_ACC(DB,fontsize=11,delta_edge=0.5,min_target_margin=20) # ,CENTER=True
W2=W.replace('\n','')
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/{W2}_2.png",dpi=300)


W="Access\n to\n information"
W2=W.replace('\n','')
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
                                  (x.source.startswith(W) and x.source.endswith(W))\
                                  or (x.target.startswith(W) and x.target.endswith(W)),\
                                      axis=1)]
# DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
#                                  (x.source.find(W)>-1 or x.target.find(W)>-1),axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0.01)
DV.plotNet(G,pos,fontsize=10,VERTEX=True,MODULARITY=True,N_M=4,loc_legend='lower center') # ,N_M=4
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/First10Papers_3.png",
            dpi=300, bbox_inches='tight')

W="availability\n and\n access\n..."
W2=W.replace('\n','')
## source
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
                                 (not x.source.startswith(W) and not x.source.endswith(W))\
                                 and (x.target.startswith(W) and x.target.endswith(W)),\
                                     axis=1)]
DV.PlotText_ACC(DB,fontsize=10,delta_edge=0.5,min_target_margin=20,CENTER=True) # ,CENTER=True
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/{W2}_target3.png",
            dpi=300, bbox_inches='tight')
## target
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
                                 (x.source.startswith(W) and x.source.endswith(W))\
                                 and (not x.target.startswith(W) and not x.target.endswith(W)),\
                                     axis=1)]
DV.PlotText_ACC(DB,fontsize=10,delta_edge=0.5,min_target_margin=20,CENTER=True) # ,CENTER=True
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/{W2}_source3.png",
            dpi=300, bbox_inches='tight')

W=("farmer\n adaptation","Climate\n Change\n Adaptation","adaptation\n climate\n change")
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]][FACTORS.apply(lambda x: \
                                 (not x.source.startswith(W) and not x.source.endswith(W))\
                                 and (x.target.startswith(W) and x.target.endswith(W)),\
                                     axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=100,delta_edge=0.05)
DV.plotNet(G,pos,fontsize=12,VERTEX=True,MODULARITY=True,N_M=4,loc_legend='lower center') 
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/adaptation2_2.png",
            dpi=300, bbox_inches='tight')


# (50,60),(60,70),(70,80),(80,90),(85,95),(110,120),(120,130)
DB=FACTORS[['ID_ART',"source","target","SIGN","proportion"]]\
    [FACTORS.ID_ART.apply(lambda x: x in range(60))] 
G,pos=DV.PosModularity(DB,dw=100,delta=50,delta_edge=0.01)
DV.plotNet(G,pos,fontsize=10,VERTEX=True,MODULARITY=True,N_M=4,loc_legend='lower center') # ,N_M=4
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/First10Papers_3.png",
            dpi=300, bbox_inches='tight')


DB=FACTORS
G,pos=DV.PosModularity(DB,dw=150,delta=100,delta_edge=0)
DV.plotNet(G,pos,fontsize=10,MODULARITY=True,
           min_target_margin=3,loc_legend=[0.9,0.1],N_M=6,VERTEX=False)
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/nClusters_2.png",dpi=300)


W="Climate\n Change\n Adaptation"
DB=FACTORS[FACTORS.apply(lambda x: \
                                 (x.source.find(W)>-1)or (x.target.find(W)>-1),\
                                     axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50)
DV.plotNet(G,pos,fontsize=12,VERTEX=True,MODULARITY=True,N_M=1)
W2=W.replace('\n','')
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/{W2}_2.png",dpi=1200)


W="Adaptation"
DB=FACTORS[FACTORS.apply(lambda x: \
                                 (x.source.find(W)>-1)or (x.target.find(W)>-1),\
                                     axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=100)
DV.plotNet(G,pos,fontsize=10,VERTEX=True,MODULARITY=True,N_M=4)
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Unsupervised/Figures/{W}_2.png",dpi=1200)


### Tryning to place in topics

DT1_FACTORS=DT1_FACTORS_ORG.copy()

DT1_FACTORS["source1"]=DT1_FACTORS.source.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
DT1_FACTORS["target1"]=DT1_FACTORS.target.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

# Creating Database with frequencies
FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','source1', 'target1','SIGN']]
FACTORS["VALUE"]=1.0
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.ADAPT_S!="" and x.ADAPT_T!="",axis=1)]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS['N_T']=len(np.unique(FACTORS.ID_ART))
N_ID_ART=Counter(FACTORS["ID_ART"])
FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1=="social\n capital" and x.target1=="climate\n change\n adaptation",axis=1)]
# Proportion is relative to the ID_ART
FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_T','N_ID_ART'],
                        as_index=False)["VALUE"].sum()
FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
FACTORS=FACTORS.drop(columns=['ID_ART','N_T','N_ID_ART'])
FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"],columns='SIGN',values='VALUE',fill_value=0)
FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS["SIGN"]=FACTORS.apply(lambda x: ["+","-","+/-"][np.argmax([x["+"],x["-"],x["+/-"]])],axis=1)
FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN],axis=1)/FACTORS["ONE"]
FACTORS=FACTORS.drop(columns=['+','-','+/-'])
FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                "ONE":"degree",
                                "source1":"source",
                                "target1":"target"})

DB=FACTORS
G,pos=DV.PosModularity(DB,dw=100,delta=50,weight="edge_weight",
                       delta_edge=0.05,resolution=1)
DV.plotNet(G,pos,fontsize=3,VERTEX=True,MODULARITY=True,min_target_margin=5)
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\\IMAGES\\Check_1.png",dpi=1200)

# Second way is to create the clusters manually 
G,pos=DV.PosModularity(DB,dw=100,delta=100)

CCA_Measures=np.unique(DT1_FACTORS.apply(lambda x: x.source if x.ADAPT_S!='' else (x.target if x.ADAPT_T!='' else ''),axis=1))
CCA_Measures=CCA_Measures[1:]

COMMU=[]
for cc,k in enumerate(CCA_Measures):
    try:
        COMMU.append({ii for ii in nx.all_neighbors(G, k)}.union({k}))
    except:
        print(k)

COMMU.sort(key=lambda x: len(x),reverse=True)

N=len(COMMU)
COMMU_r=[ii for ii in COMMU] # reversed(COMMU)
CCA_Measures_re=reversed(CCA_Measures)
COMMU2=COMMU_r.copy()
for cc,k in enumerate(COMMU_r):
    # break
    for ii in range(cc+1,N):
        # break
        COMMU2[ii]=COMMU2[ii].difference(k)

COMMU2.sort(key=lambda x: len(x),reverse=True)
COMMU2=[ii for ii in COMMU2 if len(ii)>0]
[item for sublist in COMMU2 for item in sublist]

G,pos=DV.PosModularity(DB,COMMU=COMMU2,dw=100,delta=100,delta_edge=0)
DV.plotNet(G,pos,fontsize=8,VERTEX=True,MODULARITY=True,min_target_margin=10,N_M=6) #
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\\IMAGES\\Check_2.png",dpi=1200)



''' Using the categories'''

# Creating Database with frequencies
FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','ADAPT_S', 'FACT_S',
                     'ADAPT_T', 'FACT_T','SIGN']]
FACTORS["source1"]=FACTORS.apply(lambda x: x.ADAPT_S if x.ADAPT_S!="" 
                                          else (x.FACT_S if x.FACT_S!='' else ''), axis=1)
FACTORS["source1"]=FACTORS.source1.apply(lambda x: x.strip())

FACTORS["target1"]=FACTORS.apply(lambda x: x.ADAPT_T if x.ADAPT_T!="" 
                                          else (x.FACT_T if x.FACT_T!='' else ''), axis=1)
FACTORS["target1"]=FACTORS.target1.apply(lambda x: x.strip())

FACTORS["source1"]=FACTORS.source1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
FACTORS["target1"]=FACTORS.target1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

TOBOLD=list((Counter(FACTORS.apply(lambda x: x.source1 if x.ADAPT_S=='' else '',axis=1))+\
             Counter(FACTORS.apply(lambda x: x.target1 if x.ADAPT_T=='' else '',axis=1))).keys())[1:]

FACTORS["VALUE"]=1.0
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.ADAPT_S!="" and x.ADAPT_T!="",axis=1)]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS['N_T']=len(np.unique(FACTORS.ID_ART))
N_ID_ART=Counter(FACTORS["ID_ART"])
FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1=="social\n capital" and x.target1=="climate\n change\n adaptation",axis=1)]
# Proportion is relative to the ID_ART
FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_T','N_ID_ART'],
                        as_index=False)["VALUE"].sum()
FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
FACTORS=FACTORS.drop(columns=['ID_ART','N_T','N_ID_ART'])
FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"],columns='SIGN',values='VALUE',fill_value=0)
FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS["SIGN"]=FACTORS.apply(lambda x: ["+","-","+/-"][np.argmax([x["+"],x["-"],x["+/-"]])],axis=1)
FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN],axis=1)/FACTORS["ONE"]
FACTORS=FACTORS.drop(columns=['+','-','+/-'])
FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                "ONE":"degree",
                                "source1":"source",
                                "target1":"target"})

DB=FACTORS[FACTORS.apply(lambda x: x.source!=x.target, axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50,weight="edge_weight",
                       delta_edge=0,resolution=1)
DV.plotNet(G,pos,fontsize=8,VERTEX=True,MODULARITY=True,
           min_target_margin=5,loc_legend='lower center',TOBOLD=TOBOLD)
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\\IMAGES\\NetCat.png",
            dpi=800, bbox_inches='tight')


len(G.nodes)


#### Constraining to only "adaptation"

W=('adaptat[a-z]* [a-z]*(\s|){0,2}climate change',
   'adaptat[a-z]* [a-z]*(\s|){0,2}measure',
   'adaptat[a-z]* [a-z]*(\s|){0,2}capacity',
   'farm[a-z]* [a-z]*(\s|){0,2}adaptat[a-z]*','adapt[a-z]* [a-z]*(\s|){0,2}strategy',
   'climate adaptat[a-z]*','adaptat[a-z]* [a-z]*(\s|){0,2}measure')

# Creating Database with frequencies
FACTORS=DT1_FACTORS[['Verb_lm','ID_ART','ADAPT_S', 'FACT_S',
                     'ADAPT_T', 'FACT_T','SIGN','source','target']]
FACTORS=FACTORS[FACTORS.apply(lambda x: sum([len(re.findall(w,x.source))for w in W ])==0 and
                         sum([len(re.findall(w,x.target)) for w in W ])>0,axis=1)]

FACTORS["source1"]=FACTORS.apply(lambda x: x.FACT_S if x.FACT_S!='' else '', axis=1)
FACTORS["source1"]=FACTORS.source1.apply(lambda x: x.strip())

FACTORS["target1"]=FACTORS.apply(lambda x: x.target if x.ADAPT_T!="" 
                                          else (x.FACT_T if x.FACT_T!='' else ''), axis=1)
FACTORS["target1"]=FACTORS.target1.apply(lambda x: x.strip())

FACTORS["source1"]=FACTORS.source1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))
FACTORS["target1"]=FACTORS.target1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=3))

TOBOLD=list((Counter(FACTORS.apply(lambda x: x.source1 if x.ADAPT_S=='' else '',axis=1))+\
             Counter(FACTORS.apply(lambda x: x.target1 if x.ADAPT_T=='' else '',axis=1))).keys())[1:]

FACTORS["VALUE"]=1.0
FACTORS=FACTORS[FACTORS.source1!=""]
FACTORS=FACTORS.drop_duplicates()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS['N_T']=len(np.unique(FACTORS.ID_ART))
N_ID_ART=Counter(FACTORS["ID_ART"])
FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
# FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1=="social\n capital" and x.target1=="climate\n change\n adaptation",axis=1)]
# Proportion is relative to the ID_ART
FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_T','N_ID_ART'],
                        as_index=False)["VALUE"].sum()
FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
FACTORS=FACTORS.drop(columns=['ID_ART','N_T','N_ID_ART'])
FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"],columns='SIGN',values='VALUE',fill_value=0)
FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
FACTORS=FACTORS.reset_index(drop=True)
FACTORS["SIGN"]=FACTORS.apply(lambda x: ["+","-","+/-"][np.argmax([x["+"],x["-"],x["+/-"]])],axis=1)
FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN],axis=1)/FACTORS["ONE"]
FACTORS=FACTORS.drop(columns=['+','-','+/-'])
FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                "ONE":"degree",
                                "source1":"source",
                                "target1":"target"})

DB=FACTORS[FACTORS.apply(lambda x: x.source!=x.target, axis=1)]
G,pos=DV.PosModularity(DB,dw=100,delta=50,weight="edge_weight",delta_edge=0.3,
                       resolution=1)
DV.plotNet(G,pos,fontsize=11,VERTEX=True,MODULARITY=True,min_target_margin=5,
           loc_legend='lower center',TOBOLD=TOBOLD)
plt.savefig(f"C:/Dropbox/TU_Delft/Projects/Farmers_CCA/IMAGES/NetCat_subsampl.png",
            dpi=300, bbox_inches='tight')





