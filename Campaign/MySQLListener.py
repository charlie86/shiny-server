from tweepy import Stream
from tweepy import OAuthHandler
from tweepy.streaming import StreamListener
import mysql.connector
from mysql.connector import errorcode
import time
import json 
# import sentiment_mod as s
# from twitterapistuff import *
import re

consumer_key = 'EB8kQdOppCzSMvhvMY233ZF9l'
consumer_secret_key = 'QHeuYrMnEL8I63RyQeC0pJoBzNjiNoKeNBYKSBwPEg4deiSbcO'
access_token_key = '706589608411209729-r0IRzoMhd75x3hbZ2Wj2X1UfxT6C5RC'
access_token_secret = '9mVMpoKmfnMXWGoucOtiPDp1mJkeodzDrewTtWRDsFgUQ'

keyword_list = ['HillaryClinton', 'realDonaldTrump', 'GovGaryJohnson', 'DrJillStein', 'timkaine', 'mike_pence']

cnx = mysql.connector.connect(user='chuck', password='charlie86',
                              host='tweetdb.ch74fm7hgclb.us-west-2.rds.amazonaws.com',
                              database='tweetdb',
                              charset = 'utf8mb4')

cursor = cnx.cursor()

cursor.execute('DROP TABLE IF EXISTS twitts;')
cursor.execute('CREATE TABLE IF NOT EXISTS twitts (time INT(13), username VARCHAR(255) CHARACTER SET utf8mb4, tweet VARCHAR(255) CHARACTER SET utf8mb4, trump INT, clinton INT, johnson INT, stein INT, kaine INT, pence INT);')

class listener(StreamListener):

    def on_data(self, data):
		all_data = json.loads(data)

		# check to ensure there is text in 
		# the json data
		if 'text' in all_data:
			tweet = all_data['text']
			username = all_data['user']['screen_name']
			trump = int(bool(re.search('realdonaldtrump', tweet.lower())))
			clinton = int(bool(re.search('hillaryclinton', tweet.lower())))
			johnson = int(bool(re.search('govgaryjohnson', tweet.lower())))
			stein = int(bool(re.search('drjillstein', tweet.lower())))
			kaine = int(bool(re.search('timkaine', tweet.lower())))
			pence = int(bool(re.search('mike_pence', tweet.lower())))

			cursor.execute('INSERT INTO twitts (time, username, tweet, trump, clinton, johnson, stein, kaine, pence) VALUES (%s,%s,%s,%s,%s,%s,%s,%s,%s)', (time.time(), username, tweet, trump, clinton, johnson, stein, kaine, pence))
			cnx.commit()
			print((username,tweet))

			return True

		else:
			return True

    def on_error(self, status):
        print(status)

auth = OAuthHandler(consumer_key, consumer_secret_key)
auth.set_access_token(access_token_key, access_token_secret)

def startStream():
	while True:
		try:
			twitterStream = Stream(auth, listener())
			twitterStream.filter(track=keyword_list, stall_warnings = True)
		except:
			continue
startStream()