PARZU SERVER API
----------------

By default, ParZu serves http://localhost:5003 when launched via the following command:

    ./parzu_server.py



Arguments:

    text: the raw text that you want to parse
    format: the desired output format. Suggested choices:
        [conll](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=conll)
        [prolog](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=prolog)
        [graphical](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.&format=graphical)
