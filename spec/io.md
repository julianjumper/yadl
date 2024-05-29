# Load
load "filename" as characters | commas | lines | csv | json


### characters
returns an iterator going character by character. this is useful if the data follows an unimplented structure.  


### commas
returns an iterator using `,` as a separator, _next returns a string.


### lines
returns an iterator using newline as a separator, _next returns a string.


### csv
returns an interator using newline as a separator, _next returns an iterator using `,` as a separator, _next of these iterators returns a string.


### json 
returns an iterator, _next returns a dictionary, having a key and either a string or an iterator as its value. incase the value is an iterator, _next of that iterator returns either a string (if the iterator represents a list in the json file) or a dictionary (if the iterator represents a nested dictionary in the json file) interpreting said dictionary like a json file.  