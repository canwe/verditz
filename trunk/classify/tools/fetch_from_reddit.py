import feedparser

action = "liked"
offset = 0

url = "http://programming.reddit.com/user/a9bejo/%s.rss?offset=%s"%(action, offset)

print url

feed = feedparser.parse(url)

for item in feed.entries:
  print item
