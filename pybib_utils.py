"""This is the utils.py script from pybib, found at
https://github.com/jgilchrist/pybib/blob/master/pybib/utils.py (commit
f3a6b884b56c6332e1cf3c22c8adb174c2b9f266). 

After installing pybib, I couldn't find a way to use it from within
python (the way it installed through pip, that is), so I copied the
relevant function into here.

To use, simply call pybib_utils.get_bibtex(doi). (doi must be a
string)

"""

import requests
import json

GET_URL = "http://dx.doi.org/{}"
SEARCH_URL = "http://api.crossref.org/works"

def handle_status_code(r):
    if r.status_code == 200:
        return
    elif r.status_code == 404:
        exit('Unknown')
    else:
        exit("Unhandled http response code: {}".format(r.status_code))

def search(query):
    payload = {'query': query}

    r = requests.get(SEARCH_URL, params=payload)
    r.encoding = "utf-8"

    handle_status_code(r)

    results = r.json()
    results = results["message"]["items"]
    return results

def get_bibtex(doi):
    url = GET_URL.format(doi)
    headers = {'Accept': 'application/x-bibtex; charset=utf-8'}

    r = requests.get(url, headers=headers)
    r.encoding = "utf-8"

    handle_status_code(r)

    entry = r.text.strip()
    return entry
