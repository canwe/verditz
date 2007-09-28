require "open-uri"
require "rubygems"
require_gem "feedtools"

action = "linked"
offset = 0





feed = FeedTools::Feed.open("http://programming.reddit.com/user/a9bejo/#{action}.rss?offset=#{offset}")

puts feed.items.size

#open().read
