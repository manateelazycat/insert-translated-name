import requests
import json
import sys

input = sys.argv[1]

url = 'http://localhost:11434/api/generate'

data = {
  "model": "llama2-chinese",
  "prompt": '''
  Translate the following text to English, only return the content translated, no explaination:
  {}
'''.format(input)
}

result = ""
with requests.post(url, json=data, stream=True) as response:
    for line in response.iter_lines():
        if line:
            json_response = json.loads(line)
            if not json_response["done"]:
                result += json_response["response"]

print(result, flush=True)
