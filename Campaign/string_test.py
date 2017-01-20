import re

words = ['this is shit', 'i am tits']

bools = []

for i in words:
	bools.append(bool(re.search('shit', i)))

print bools