#!/usr/bin/env python
# coding: utf-8

# In[2]:


import requests
import pandas as pd
import numpy as np
import json

# Below is a sample script for your submissions.
# We advise you to play around with the code and read it carefully.
# Feel free to grab the utility functions below and experiment with them yourself.
# We want to remind you that you can submit more than once, and we will use the latest one.
# IMPORTANT: Below, you need to fill your `predict` function for your predictions
# and add your username & password in the space below.
# Set submit_now to false if you are experimenting and just want to see the result of your predictions.
# Set to True if you actually want to submit.

URL = 'http://46.101.121.83'

USERNAME = "Miners"
PASSWORD = "NsY7hhlU9zjl8DH3"
submit_now = True  # Set this to True if you want to submit your predictions.


def predict():
  ### YOUR CODE GOES HERE
  """
    Students are expected to fill this method.
    :return: A list of floats with length 24
    """

  predictions = list(y_pred.reshape(-1))
  ### YOUR CODE ENDS HERE
  print(predictions)  # Should be a dictionary of forecasts
  # i.e. {"id1" : forecast, "id2": forecast, ...}
  return predictions


### CODE BY THE TEACHING STAFF BEGINS HERE - YOU DO NOT NEED TO CHANGE###


def get_token(username, password):
  body = {"username": username, "password": password}
  r = requests.post(f'{URL}/token/', data=body)
  print(r)
  print(r.text)
  r = r.json()
  token = r["key"]
  return token

def check_format(predictions):
  assert isinstance(predictions, list)
  for i, pred in enumerate(predictions):
    try:
      predictions[i] = float(pred)
    except:
      error_str = f"Your prediction for index = {i} is not a numerical value. Please cast it to either native Python int/floats, or np.number subtype before submission."
      raise ValueError(error_str)


def send_submission(predictions: dict, token: str, submit_now: bool):
  check_format(predictions)
  submission = predictions
  print(f"Your submission will be : {submission}")

  if not submit_now:
    print("You did not submit.")
    return

  submission_body = {"submission": json.dumps(submission)}
  header = {'Authorization': f'Token {token}'}
  r = requests.post(f'{URL}/submission/', data=submission_body, headers=header)

  if r.status_code == 201:
    print(
        "Successfully submitted. Below you can see the details of your submission"
    )

  else:
    print(
        "Could not submit. Please check the error message below, contact the assistant if needed."
    )

  r = r.json()
  print(r)


if __name__ == "__main__":
  ### YOUR CODE GOES HERE
  username = USERNAME
  password = PASSWORD
  ### YOUR CODE ENDS HERE
  token = get_token(username, password)
  

### CODE BY THE TEACHING STAFF ENDS HERE - YOU DO NOT `NEED` TO CHANGE ###


# In[7]:


import pandas as pd
import tensorflow as tf
from tensorflow import keras
from keras.models import Sequential
from keras.layers import Flatten, LSTM
from keras.layers import Activation, Dense
from sklearn.model_selection import train_test_split
import numpy as np


# In[8]:


df = pd.read_csv("C:/Users/bahad/GitHub/582project/bahadir/IE582_Fall20_ProjectTrain.csv")
df['y'] = df['y'].astype("category").cat.codes


# In[9]:


# properties = list(df.columns.values)
# properties.remove('y')
# X = df[properties]
# y = df['y']


# In[10]:


# X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=0)

# model = Sequential()
# model.add(Dense(60, activation='relu', input_shape=(60,)))

# #model.add(Dense(32, activation='relu'))

# model.add(Dense(16, activation='relu'))

# model.add(Dense(1, activation='sigmoid'))

# print(model.summary())

# model.compile(optimizer='adam',
#               loss='binary_crossentropy',
#               metrics=['accuracy'])

# model.fit(X_train, y_train, epochs=50, batch_size=4)

# test_loss, test_acc = model.evaluate(X_test, y_test)
# print('Test accuracy:', test_acc)


# # For Normal Dataset

# In[11]:


final_test=pd.read_csv("C:/Users/bahad/GitHub/582project/bahadir/IE582_Fall20_ProjectTest.csv")


# In[12]:


properties = list(final_test.columns.values)
properties.remove('y')
X_test = final_test[properties]
y_test = final_test['y']


# In[13]:


df['y'] = df['y'].astype("category").cat.codes
properties = list(df.columns.values)
properties.remove('y')
X_train = df[properties]
y_train = df['y']


# In[23]:


model = Sequential()
model.add(Dense(60, activation='relu', input_shape=(60,)))

model.add(Dense(32, activation='relu'))

model.add(Dense(16, activation='relu'))

model.add(Dense(16, activation='relu'))

model.add(Dense(1))

print(model.summary())

model.compile(optimizer='adam',
              loss='mean_squared_error',
              metrics=['accuracy']
             )

model.fit(X_train, y_train, epochs=100, batch_size=8)


# ## Prediction

# In[32]:


y_pred = model.predict_classes(X_test)


# In[ ]:


prediction = y_pred
send_submission(prediction, token, submit_now)

