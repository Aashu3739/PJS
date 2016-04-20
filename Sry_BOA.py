import nltk
import csv
import re
import glob
import os
from nltk import pos_tag, ne_chunk
import nltk.tag, nltk.data
from nltk.tokenize import SpaceTokenizer

os.chdir("D:/R/BOA/txtfiles")
for fileName in glob.glob("*.txt"):
    count=0
    file = open('D:/R/BOA/txtfiles/'+fileName, 'r')
    filew = open('D:/R/BOA/Noun/'+fileName, "wb")
    for line in file:
                                    count=count+1
                                    print count
                                    print line
                                    line = re.sub('\\f', '', line)
                                    #line = line.decode("utf-8")
                                    line = unicode(line, errors='ignore')
                                    tokenizer = SpaceTokenizer()
                                    toks = tokenizer.tokenize(line)
                                    default_tagger = nltk.data.load(nltk.tag._POS_TAGGER)
                                    model = {'Consumer': 'RB'}
                                    tagger = nltk.tag.UnigramTagger(model=model, backoff=default_tagger)
                                    #pos = pos_tag(toks)
                                    pos=tagger.tag(toks)
                                    print pos
                                    chunked_nes = ne_chunk(pos) 
                                    nes = [' '.join(map(lambda x: x[0], ne.leaves()))
                                           for ne in chunked_nes
                                                 if isinstance(ne, nltk.tree.Tree)]
                                            #data.append(nes)
                                    print nes 
                                    filew.write((','.join(nes))+'\n')
                                    #filew.write("yeah its me")
    
    
    
    filew.close()             
    file.close()