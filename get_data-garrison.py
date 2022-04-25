## script prepared for Bill @ Hudson Bay Ski resort

import io
import pandas as pd
import requests

response = requests.get('https://datagarrison.com/users/300234062103550/300234065724550/temp/10746708_006.txt').text
csv = pd.read_csv(io.StringIO(response.replace('"\t"', '"\n"')), delimiter='\t', skiprows=[0,1])
csv.to_csv('hudsonbaymtn.csv')
csv.iloc[-1] # returns the last row / most recent observations