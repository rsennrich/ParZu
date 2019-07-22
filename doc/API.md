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
- inputformat: inputformat: the format of the input string. Valid choices:
   - [plain](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner.Er%20ist%20ein%20Hamburger.&format=conll&inputformat=plain)
   - [tokenized](http://localhost:5003/parse?text=Ich%0Abin%0Aein%0ABerliner%0A.%0A%0AEr%0Aist%0Aein%0AHamburger%0A.&format=conll&inputformat=tokenized)
   - [tokenized_lines](http://localhost:5003/parse?text=Ich%20bin%20ein%20Berliner%20.%0AEr%20ist%20ein%20Hamburger%20.&format=conll&inputformat=tokenized_lines)
   - [tagged](http://localhost:5003/parse?text=Ich%20PPER%0Abin%20VAFIN%0Aein%20ART%0ABerliner%20NN%0A.%20$.%0A%0AEr%20PPER%0Aist%20VAFIN%0Aein%20ART%0AHamburger%20NN%0A.%20$.&format=conll&inputformat=tagged)
