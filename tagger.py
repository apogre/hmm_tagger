data_set = []
states=[]
obsvs=[]

with open("entrain.txt","r") as f:
	content = f.readlines()
	for c in content:
		data=c.split('/')
		state=data[0]
		obs=data[1]
		states.append(state)
		obsvs.append(obs)
		print state
		print obs
		print len(states)
		print len(obsvs)
# 		relation = dict({'state':state,'obs':obs})
# 		data_set.append(relation)
		

# for item in data_set:
# 	for k,v in item.iteritems():
# 		print k
# 		print v
# 		abc = input()