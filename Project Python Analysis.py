#!/usr/bin/env python
# coding: utf-8

# In[3]:


import pandas as pd
import numpy as np
from pandas_profiling import ProfileReport


# In[4]:


df=pd.read_csv('IE582_Fall20_ProjectTrain.csv')
profile = ProfileReport(df[df.columns[0:61]], explorative=True)


# In[5]:


profile


# # Test Data

# In[6]:


df_test=pd.read_csv("IE582_Fall20_ProjectTest.csv")
test_profil = ProfileReport(df_test, explorative=True)


# In[8]:


test_profil


# In[ ]:




