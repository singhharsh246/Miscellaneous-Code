from pymongo import MongoClient
from pprint import pprint 



def get_database():
	client = MongoClient("localhost", 27017)
	db = client.database_name
	collection = db.collection_name
	pt = collection.find_one({"name":"samples_pokemon"})
	db = client["samples_pokemon"]
	col = db['samples_pokemon']

	
	print("Part 1 of the question \n")

	cursor = col.find({'candy_count':{'$gte': 7 }})
    	
	for document in cursor:
		print(document['name'])

	print("Part 2 of the question \n")


	cursor2 = col.find({'$or': [ { 'num':  '3' } , { 'num' : '4' } ] })

	for document in cursor2:
		print(document['name'])


if __name__ == "__main__":
	get_database()
	
