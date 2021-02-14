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

USERNAME = "your_user"
PASSWORD = "your_password"
submit_now = True  # Set this to True if you want to submit your predictions.


def predict():
  ### YOUR CODE GOES HERE
  """
    Students are expected to fill this method.
    :return: A list of floats with length 24
    """

  predictions = list(np.random.randint(low=0, high=1, size=2073))
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
  prediction = predict()
  send_submission(prediction, token, submit_now)

### CODE BY THE TEACHING STAFF ENDS HERE - YOU DO NOT `NEED` TO CHANGE ###
