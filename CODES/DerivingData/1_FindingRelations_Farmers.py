
"""
##################################
# 
# Author: Dr. Sofia Gil-Clavel
# 
# Last update: October 30th, 2024.
# 
# Description: Functions to process the data used in the article:
# Gil-Clavel, S., Wagenblast, T., & Filatova, T. (2023, November 24). Incremental
# and Transformational Climate Change Adaptation Factors in Agriculture Worldwide:
# A Natural Language Processing Comparative Analysis. https://doi.org/10.31235/osf.io/3dp5e
# 
# Computer Environment:
#   - Windows 
#   - Microsoft Windows 10 Enterprise
#   - Python 3.11
# 
# Conda Environment to run the code:
#   - @SofiaG1L/FloodSLR_CCA/PY_ENVIRONMENT/pytorch_textacy.yml
#
# Conda Environments that have to be installed in the computer:
#   - @SofiaG1L/Database_CCA/PY_ENVIRONMENTS/EXTRA/neuralcoref2.yml
#   - @SofiaG1L/Database_CCA/PY_ENVIRONMENTS/EXTRA/scispacy.yml
# 
##################################
"""

import os as os
import glob

import pickle
import subprocess

# Data Handling
import pandas as pd
from tqdm import tqdm
tqdm.pandas(desc="my bar!")

# Basic Libraries
import numpy as np
from collections import Counter
import random

# Processing text
import nltk as nltk
import regex as re

# Import Text Processing Libraries
import spacy as spacy

# Plots 
# from wordcloud import WordCloud
from matplotlib import pyplot as plt
from matplotlib import patches

# =============================================================================
# Functions
# =============================================================================
os.chdir("@SofiaG1L/NLP4LitRev//MainFunctions/")

import Functions as FN
import DataViz as DV
import FindTextPatterns as PTN

# =============================================================================
# Openning the data and keeping only SLR and flood related articles
# =============================================================================

## Opening the data that results from:
##      "@SofiaG1L/Database_CCA/CODES/3_ProcessingDataframe.py"
with open('@SofiaG1L/Database_CCA/PROCESSED/df2.pickle', 'rb') as handle:
    DT1 = pickle.load(handle)

DT1["text2"]=DT1.apply(lambda x: x["dc:title"]+"\n"+x["description"],axis=1)

DT1=DT1[DT1.text2.apply(lambda x: len(re.findall("agricultur[a-z]*",x))>0 or 
                  len(re.findall("farm[a-z]*",x))>0)].copy()

DT1=DT1.reset_index(drop=True)

#### Further cleaning the text ####
DT1["text2"]=DT1.apply(lambda x: x["description"]+"\n"+x.analysis+"\n"+x.results+"\n"+
                       x.findings+"\n"+x.conclusions+"\n"+x.discussion,axis=1)

#### Further cleaning the text ####

DT1["text_clean"]=DT1["text2"].progress_apply(lambda x: FN.CleanText(x))

# =============================================================================
# Openning the models
# =============================================================================
nlp0 = spacy.load("@SofiaG1L/Database_CCA/PROCESSED/MODELS/model-findings")
nlp1 = spacy.load("@SofiaG1L/Database_CCA/PROCESSED/MODELS/model-nouns") # This model is better at finding the nouns. It is en_core_sci_lg updated
nlp2 = spacy.load('en_core_web_trf') # This model is better to find markers

### Detect all sentences
DT1["sentence"] = DT1["text_clean"].progress_apply(lambda x: list(map(nltk.sent_tokenize,[x]))[0])

# =============================================================================
# Extract (Subject, Verb, Object) from the findings
# =============================================================================
VERBS_dict=PTN.SignDict("@SofiaG1L/Database_CCA/Database_CCA/DATA/Verbs.csv")

### Split sentences into (Subject, Verb, Object)
DT1["SVAOS"]=DT1.sentence.progress_apply(lambda x: [PTN.FindAllSents(ii, nlp1, nlp2, VERBS_dict) 
                                                    for ii in x if PTN.Finding(ii,nlp0)])
### Flatten list of lists
DT1["SVAOS"]=DT1["SVAOS"].progress_apply(lambda x: [item for sublist in x for item in sublist])

### Keeping only articles that have some (Subject, Verb, Object) sentences
DT1=DT1[DT1["SVAOS"].apply(lambda x: len(x)!=0)]
DT1=DT1.reset_index(drop=True)

DT1[DT1.apply(lambda x: any(x[0].find("armed conflict")>-1 for x in x.SVAOS),axis=1)].index
DT1[DT1.apply(lambda x: any(x[2].find("buy - out")>-1 or x[2].find("buyout")>-1 or 
                            x[2].find("buy out")>-1 for x in x.SVAOS),axis=1)].index

# =============================================================================
# Replacing Subjects and Objects with Umbrella Categories
# =============================================================================

## Floods adaption strategies
ADAPT=pd.read_csv("@SofiaG1L/Farmers_CCA/DATA/Farmers_AdaptationOptions2_python.csv",sep = ",")
ADAPT=ADAPT.rename(columns={"Adaptation_Strategy":"Adaptation_Name","Adaptation_Strategy_RGX":"Adaptation_REGEX"})

## Farmers' factors for FACTation
FACT=pd.read_csv("@SofiaG1L/Farmers_CCA/PROCESSED/Factors_Dictionary_python_SGCTF.csv",sep = ",")
FACT=FACT.rename(columns={"Tatiana_Factor_Strategy":"Factor_Name","Factor_Strategy_RGX":"Factor_REGEX"})

import warnings
warnings.simplefilter(action='ignore', category=FutureWarning)

DT1["ADAPT"]=DT1.SVAOS.progress_apply(lambda x: FN.LabelMeasures(x,ADAPT,FACT))

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_20240930.pickle', 'wb') as handle:
#     pickle.dump(DT1, handle, protocol=pickle.HIGHEST_PROTOCOL)

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_20240930.pickle', 'rb') as handle:
#     DT1 = pickle.load(handle)

# =============================================================================
# ADAPT1->ADAPT2 & ADAPT2->FACT => ADAPT1->FACT
# FACT1->FACT2 & FACT2->ADAPT => FACT1->ADAPT
# =============================================================================

def IfABthenBC(SENT):
    N=len(SENT)
    ADD=[]
    for ii in range(N):
        if SENT[ii][2][1]!="" and SENT[ii][0][1]==SENT[ii][2][1]:
            A=SENT[ii][0][0]
            B=SENT[ii][2][0]
            if A!=B:
                for jj in range(ii+1,N):
                    C=SENT[jj][0][0]
                    D=SENT[jj][2][0]
                    if (B==C) and (SENT[jj][0][1]!=SENT[jj][2][1]) and (SENT[jj][2][1]!=""):
                        SIGN=FN.FriendOfFriend(SENT[ii][1],SENT[jj][1])
                        ADD.append(((A,SENT[ii][0][1]),SIGN,(D,SENT[jj][2][1])))
    return(ADD)

CHECK=DT1.ADAPT.progress_apply(IfABthenBC)

DT1["ADAPT2"]=DT1.ADAPT+CHECK

# =============================================================================
# Keeping only those sentences that talk about CCA measures and/or factors
# =============================================================================
key="ADAPT2"
DT1_FACTORS=DT1.explode(key)
DT1_FACTORS[["NOUNA","SIGN","NOUNB"]]=pd.DataFrame(DT1_FACTORS[key].tolist(), index= DT1_FACTORS.index)
DT1_FACTORS[["source","source_type"]]=pd.DataFrame(DT1_FACTORS.NOUNA.tolist(), index= DT1_FACTORS.index)
DT1_FACTORS[["target","target_type"]]=pd.DataFrame(DT1_FACTORS.NOUNB.tolist(), index= DT1_FACTORS.index)
DT1_FACTORS=DT1_FACTORS[DT1_FACTORS.apply(lambda x: x.source_type!="" and x.target_type!="",axis=1)]
DT1_FACTORS=DT1_FACTORS.reset_index(drop=True)

TOBOLD=Counter(DT1_FACTORS.NOUNA.apply(lambda x: x[0] if x[1]=="ADAPT" else ""))+\
    Counter(DT1_FACTORS.NOUNB.apply(lambda x: x[0] if x[1]=="ADAPT" else ""))
TOBOLD.most_common()

TOBOLD=Counter(DT1_FACTORS.NOUNA.apply(lambda x: x[0] if x[1]=="FACT" else ""))+\
    Counter(DT1_FACTORS.NOUNB.apply(lambda x: x[0] if x[1]=="FACT" else ""))
TOBOLD.most_common()

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_FACTORS_20240930.pickle', 'wb') as handle:
#     pickle.dump(DT1_FACTORS, handle, protocol=pickle.HIGHEST_PROTOCOL)

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_FACTORS_20240930.pickle', 'rb') as handle:
#     DT1_FACTORS = pickle.load(handle)

# =============================================================================
# How to extract meaningful relations?
# Check if sentences are the same based on permutations 
# =============================================================================

''' Using the categories''' #### Move Here N2

def PlotGraphNet(DB,TOBOLD,FONT=None,COLORStat=None,COLORS=None,delta_edge=1,
                 resolution=1,N_M=None,edge_sigmoidA=False,edge_sigmoid=False,
                 q=0.75,k=100,L=10,VERTEX=True):
    # Creating Database with frequencies
    FACTORS=DB.copy()
    
    FACTORS["source1"]=FACTORS.source.apply(lambda x: x.strip())
    FACTORS["target1"]=FACTORS.target.apply(lambda x: x.strip())
    
    FACTORS["source1"]=FACTORS.source1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=4))
    FACTORS["target1"]=FACTORS.target1.map(lambda x: DV.AddBreakLine(x,n=1,breakAfter=4))
    
    FACTORS["VALUE"]=1.0
    FACTORS=FACTORS[FACTORS.apply(lambda x: x.source1!=x.target1,axis=1)]
    FACTORS=FACTORS.reset_index(drop=True)
    N_T=len(np.unique(FACTORS.ID_ART))
    N_ID_ART=Counter(FACTORS["ID_ART"])
    FACTORS['N_ID_ART']=[N_ID_ART[x] for x in FACTORS['ID_ART']]
    # Proportion is relative to the ID_ART
    FACTORS=FACTORS.groupby(['ID_ART','source1', 'target1','SIGN','N_ID_ART'], #,'N_T'
                            as_index=False)["VALUE"].sum()
    FACTORS=FACTORS.assign(VALUE= lambda x: x.VALUE/x.N_ID_ART,ONE=1.0)
    FACTORS=FACTORS.drop(columns=['ID_ART','N_ID_ART']) #,'N_T'
    FACTORS=FACTORS.groupby(['source1', 'target1','SIGN'],as_index=False).sum()
    FACTORS=FACTORS.pivot_table(index=['source1', 'target1',"ONE"], #
                                columns='SIGN',values='VALUE',fill_value=0)
    FACTORS=FACTORS.rename_axis(None, axis=1).reset_index()
    FACTORS=FACTORS.groupby(['source1', 'target1'],as_index=False).sum()
    SIGNS=[x for x in FACTORS.columns if x not in ["source1","target1","ONE"]] #
    FACTORS["SIGN"]=FACTORS.apply(lambda x: SIGNS[np.argmax(x[SIGNS])],axis=1)
    FACTORS["VALUE"]=FACTORS.apply(lambda x: x[x.SIGN]*x.ONE,axis=1)
    if edge_sigmoidA:
        FACTORS["VALUE2"]=DV.QuantilesWeight_upQ(list(FACTORS["VALUE"]),q=q)
        FACTORS=FACTORS[FACTORS.VALUE2.apply(lambda x: x>0)]
        FACTORS=FACTORS.reset_index(drop=True)
    FACTORS=FACTORS.rename(columns={"VALUE":"edge_weight",
                                    "ONE":"degree",
                                    "source1":"source",
                                    "target1":"target"})
    
    FACTORS=FACTORS[FACTORS.apply(lambda x: x.source!=x.target, axis=1)]
    FACTORS=FACTORS[FACTORS.apply(lambda x: x.source!="" and x.target!="",axis=1)]
    FACTORS=FACTORS.reset_index(drop=True)
    G,pos=DV.PosModularity(FACTORS,dw=100,delta=50,weight="edge_weight",
                           delta_edge=delta_edge,resolution=resolution)
    DV.plotNet(G,pos,fontsize=10,VERTEX=VERTEX,MODULARITY=True,
               loc_legend='lower center',TOBOLD=TOBOLD)


TOBOLD=Counter(DT1_FACTORS.apply(lambda x:  DV.AddBreakLine(x.source,n=1,breakAfter=4) if x.NOUNA[1]=="FACT" else "",axis=1))+\
    Counter(DT1_FACTORS.apply(lambda x: DV.AddBreakLine(x.target,n=1,breakAfter=4) if x.NOUNB[1]=="FACT" else "",axis=1))
TOBOLD.most_common()

REMOVE=['Climate Change Adaptation','Climate Change Maladaptation',
          'Climate Change Transformational Adaptation',
          'Climate Change Incremental Adaptation',"TCCA-specific ",
          "Adaptive Capacity","Health","Network","Local Context"]


### All links considered
DB=DT1_FACTORS.copy()
DB=DB[DB.apply(lambda x: x.source not in REMOVE and x.target not in REMOVE,axis=1)]
DB=DB.reset_index(drop=True)
DB=DB[['dc:identifier','source', 'target','SIGN']]
DB=DB.rename(columns={'dc:identifier':'ID_ART'})
PlotGraphNet(DB,list(TOBOLD.keys()),FONT=12,COLORStat=None,COLORS=None,VERTEX=True,
             delta_edge=5,edge_sigmoidA=False,edge_sigmoid=True,q=0.75,L=5,N_M=None)
plt.savefig("C:\\Dropbox\\TU_Delft\\Projects\\Farmers_CCA\IMAGES\\NetCat_FarmersAll_20240927.svg",
            dpi=300, bbox_inches='tight')


### Only MEASURE - FACTOR
DB=DT1_FACTORS.copy()
DB=DB[DB.apply(lambda x: x.source not in REMOVE and x.target not in REMOVE,axis=1)]
DB=DB[DB.apply(lambda x: x.source_type!=x.target_type,axis=1)]
DB=DB.reset_index(drop=True)
DB=DB[['dc:identifier','source', 'target','SIGN']]
DB=DB.rename(columns={'dc:identifier':'ID_ART'})
PlotGraphNet(DB,list(TOBOLD.keys()),FONT=12,COLORStat=None,COLORS=None,
             delta_edge=1.5,edge_sigmoidA=False,q=0.75,L=5,N_M=None,
             edge_sigmoid=False,VERTEX=True)
# plt.savefig("@SofiaG1L/Farmers_CCA/IMAGES/NetCat_20240927.svg",
#             dpi=300, bbox_inches='tight')



# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_FACTORS_20240913.pickle', 'wb') as handle:
#     pickle.dump(DT1_FACTORS, handle, protocol=pickle.HIGHEST_PROTOCOL)

# DT1_FACTORS.to_csv("@SofiaG1L/Farmers_CCA/PROCESSED/DT1_FACTORS_20240913.csv")

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_FACTORS_20240913.pickle', 'rb') as handle:
#     DT1_FACTORS2 = pickle.load(handle)

# with open('@SofiaG1L/Farmers_CCA/PROCESSED/DT1_20240913.pickle', 'wb') as handle:
#     pickle.dump(DT1, handle, protocol=pickle.HIGHEST_PROTOCOL)

# DT1.to_csv("@SofiaG1L/Farmers_CCA/PROCESSED/DT1_20240913.csv")







