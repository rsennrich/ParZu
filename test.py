import parzu_server as parzu
options = parzu.process_arguments()

print options
ParZu = parzu.Server(options)

a = ParZu.tokenize('Das ist ein Test. Das auch.')
print a
b = ParZu.tag(a)
print b
c = ParZu.preprocess(b)
print c
