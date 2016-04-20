import nltk
import csv

from nltk import pos_tag, ne_chunk
import nltk.tag, nltk.data
from nltk.tokenize import SpaceTokenizer


with open("D:/R/email_Analysis/FINAL/pyhton_mssg.csv", "r") as csvfile:
        datareader = csv.reader(csvfile,quotechar='"' ,lineterminator='\n',quoting=csv.QUOTE_ALL)
        csv_out = open('D:/R/email_Analysis/FINAL/Noun.csv.csv', 'wb')
	mywriter = csv.writer(csv_out)
	count=0
	for row in datareader:
				count = count + 1
				print "COUNT is :%d" % count
				tokenizer = SpaceTokenizer()
                                toks = tokenizer.tokenize((''.join(row)))
				default_tagger = nltk.data.load(nltk.tag._POS_TAGGER)
				model = {'Almost': 'RB','shikha':'NNP','Lots':'','bbnt':'NNP','Swati':'NNP','Sarkar':'NNP','Deepak':'NNP','Capgemini':'NNP','Swati':'NNP','Deepak Shete':'NNP','Melini':'NNP','Lots':'RB','Prashant Deshpande':'NNP','Deepak A. Shete':'NNP','Rajesh Achyut Patankar':'NNP','Shailesh V. Naik':'NNP','Prashant':'NNP','Kuldeep Vishnu Deshpande':'NNP','Kuldeep Deshpande':'NNP','Hi':'UH','From':'IN','Subject':'VB','RE':'SYM','Cc':'SYM','CC':'SYM','Start':'RB','All':'RB','PLEASE':'RB','Request':'RB','Add':'RB','Need':'RB','Completed':'VB','To':'RB','Dear':'RB','Thank':'RB','You':'PRP','We':'PRP','Here':'RB','Team':'RB','Please':'UH','Thanks':'UH','Regards':'UH','See':'VB','Test':'VB','ASAP':'SYM','Sent':'VB','mailto':'SYM','Together':'RB','Is':'VB','AS':'RB','Financial Services Strategic Business Unit':'NNP','fax':'RB','mobile':'RB','except':'RB','date':'RB','new':'RB','courier':'RB','extn':'RB'}
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
                                mywriter.writerow(nes)
                                
        csv_out.close()	                            

	  
