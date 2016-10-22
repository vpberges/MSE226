# This crawler is used to extract the data from the payscale website
#Author: Vincent-Pierre Berges
#Date: 2016-10-15

import pandas as pd


s = open('Best Universities and Colleges _ Payscale.htm').read()
cols = ['University','earlyCareer','midCareer']
table = pd.DataFrame(columns =cols)



start = 0


while True :
	try:
		first = '</span></span></td><td class="hidden-xs hidden text-center" data-reactid=".0.0.1.0.0.0.1.$'
		last = 'undefined.0:$'
		start = s.index( first , start+1) + len( first )
		end = s.index( last, start )
		name =  s[start:end]

		first = 'undefined_Early Career Median Pay.0.0">$'
		last='</span>'
		start = s.index( first , start+1) + len( first )
		end = s.index( last, start )
		early = int(s[start:end].replace(',',''))

		first = 'undefined_Mid-Career Median Pay.0.0">$'
		last='</span>'
		start = s.index( first , start+1) + len( first )
		end = s.index( last, start )
		mid= int(s[start:end].replace(',',''))

		table = table.append(pd.DataFrame([[name,early,mid]],columns = cols)) 
		table.to_csv('extracted.csv')

	except:
		break
