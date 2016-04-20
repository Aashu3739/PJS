import nltk
import csv

from nltk import pos_tag, ne_chunk
import nltk.tag, nltk.data
from nltk.tokenize import SpaceTokenizer
from nltk import sent_tokenize, word_tokenize, pos_tag, ne_chunk
 
 
def extract_entities(text):
	entities = []
	for sentence in sent_tokenize(text):
            tokenizer = SpaceTokenizer()
            toks = tokenizer.tokenize(sentence)
            default_tagger = nltk.data.load(nltk.tag._POS_TAGGER)
            #model = {'_': 'RB','shikha':'NNP','Lots':'','bbnt':'NNP','Swati':'NNP','Sarkar':'NNP','Deepak':'NNP','Capgemini':'NNP','Swati':'NNP','Deepak Shete':'NNP','Melini':'NNP','Lots':'RB','Prashant Deshpande':'NNP','Deepak A. Shete':'NNP','Rajesh Achyut Patankar':'NNP','Shailesh V. Naik':'NNP','Prashant':'NNP','Kuldeep Vishnu Deshpande':'NNP','Kuldeep Deshpande':'NNP','Hi':'UH','From':'IN','Subject':'VB','RE':'SYM','Cc':'SYM','CC':'SYM','Start':'RB','All':'RB','PLEASE':'RB','Request':'RB','Add':'RB','Need':'RB','Completed':'VB','To':'RB','Dear':'RB','Thank':'RB','You':'PRP','We':'PRP','Here':'RB','Team':'RB','Please':'UH','Thanks':'UH','Regards':'UH','See':'VB','Test':'VB','ASAP':'SYM','Sent':'VB','mailto':'SYM','Together':'RB','Is':'VB','AS':'RB','Financial Services Strategic Business Unit':'NNP','fax':'RB','mobile':'RB','except':'RB','date':'RB','new':'RB','courier':'RB','extn':'RB'}
	    model =  {'extn':'RB'}
            tagger = nltk.tag.UnigramTagger(model=model, backoff=default_tagger)
            pos = pos_tag(toks)
            pos=tagger.tag(toks)
            #print pos
            chunks = ne_chunk(pos) 
	    #chunks = ne_chunk(pos_tag(word_tokenize(sentence)))
	    entities.extend([chunk for chunk in chunks if hasattr(chunk, 'node')])
	return entities



#with open("D:/R/BOA/PySrc/FGD1_18-25_Vodafone_Prepaid_BCUsers_Mumbai.csv", "r") as csvfile:
        datareader = csv.reader(csvfile,quotechar='"' ,lineterminator='\n',quoting=csv.QUOTE_ALL)
        csv_out = open('D:/R/BOA/Noun/FNoun.csv', 'wb')
	mywriter = csv.writer(csv_out)
	count=0
	for row in datareader:
				count = count + 1
				print "COUNT is :%d" % count
                                print row(''.join(row))
				#mywriter.writerow(extract_entities(''.join(row)))


	#csv_out.close()	
	
	file = open('D:/R/BOA/txtfiles/FGD1_18-25_Vodafone_Prepaid_BCUsers_Mumbai.txt', 'r')
	print file.read()
	filew = open('D:/R/BOA/Noun/FNoun.txt', "w")
	for line in file:
                                print line
                                filew.write(extract_entities(line))
                                #filew.write("yeah its me")



        filew.close()
	
