PARZU SERVER API
----------------

By default, ParZu serves http://localhost:5003 when launched via the following command:

    ./parzu_server.py

The main app is [parse](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner), which supports POST and GET requests.
The following arguments are implemented:

- text: the raw text that you want to parse. Required.
- format: the desired output format. Default: conll format. Suggested choices:
   - [conll](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=conll)
   - [prolog](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=prolog)
   - [graphical](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=graphical)
