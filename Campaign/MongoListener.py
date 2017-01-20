import time
from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener
from pymongo import MongoClient
import json

consumer_key = 'EB8kQdOppCzSMvhvMY233ZF9l'
consumer_secret_key = 'QHeuYrMnEL8I63RyQeC0pJoBzNjiNoKeNBYKSBwPEg4deiSbcO'
access_token_key = '706589608411209729-r0IRzoMhd75x3hbZ2Wj2X1UfxT6C5RC'
access_token_secret = '9mVMpoKmfnMXWGoucOtiPDp1mJkeodzDrewTtWRDsFgUQ'

mongo_host = 'localhost'
mongo_port = 27017

keyword_list = ['HillaryClinton', 'realDonaldTrump', 'GovGaryJohnson', 'DrJillStein']

class MongoListener(StreamListener):
	def __init__(self, start_time, time_limit = 60):

		self.time = start_time
		self.limit = time_limit

	def on_status(self, status):
		print '[on_status] status = ' + status.text

	def on_data(self, data):

		while (time.time() - self.time) < self.limit:

			try:
				print time.time()
				client = MongoClient(mongo_host, mongo_port)
				db = client['twitter_db']
				collection = db['twitter_collection']

				tweet = json.loads(data)
				collection.insert(tweet)
				return True

			except BaseException, e:

				print 'failed on data,', str(e)
				print data
				time.sleep(5)
				pass

		exit()
	def on_error(self, status):
		print '[on_error] ' + status

auth = OAuthHandler(consumer_key, consumer_secret_key)
auth.set_access_token(access_token_key, access_token_secret)

twitterStream = Stream(auth, MongoListener(start_time = time.time(), time_limit = 2000000))
twitterStream.filter(track = keyword_list)