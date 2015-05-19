The Internet has become a collective knowledge space in our
society. Each piece of content added to this space, if it is produced
by professional researchers or by dedicated amateurs, can be a useful
answer, depending on the question that is being asked. There are
various applications on the Internet that aim to filter the
interesting information from the huge amount of available content. Of
course, this raises the question what makes a site interesting to a
particular user.


The by far most popular approach is to equal 'interesting' with
'popular', a quality that is often more easy to find out with an
algorithm. For example, search engines use the links between pages as
votes and sort search results by these votes. Similar, social news
aggregators let users vote on all submissions and use these votes to
determine the 'hot' topics. This approach works very good as long as
the users views and opinions match the opinions of the masses.
Because this is often not the case, the next generation of news
filters work on personalizing the results by evaluating the users
behavior.


Both approaches have their drawbacks: Ranking results by popularity
makes it difficult to find information that is not already widely
accepted. A strong personalized result-set can have the effect that
the user is less confronted with new ideas and topics, because he only
sees pages that he is already interested in. As an easy way to avoid
this, a good information filter could mix a few randomly selected
pages into the result sets.



Ranking by popolarity and by user preference do work quite nicely
together. As an example, the social news aggregator reddit allows
users to submit arbitrary web pages to the service and then readers
determine the order of the topics by up- or downvoting these
postings. Additionally, reddit also offers an alternative view of the
submitted posts, this time sorted by the users history of votes.


## An Overview of the Verditz Information Filtering System ##


Verditz is a web application that collects popular web pages such as collected
by an already existing news aggregator and tries to filter out the pages that
matches the users previous interests. This is done by using only machine
learning algorithms. Users may use Verditz as a service to read about the topics
currently discussed on the web, but filtered by the pages the actually like.

### Goals: ###

The main goal is to recommend users articles from their rss feeds, based on their feedback on articles they already read.
The service will crawl pages from an predefined set of RSS feeds, as well as accept individual submissions by a
registered user. For the creation of verditz, we will use a set of randomly chosen RSS feeds as an input.
There will be an overview of new submissions, ordered by time. Users
may up or downvote any of these submissions to decide of the like or dislike an
article. These votes will then be used to classify the unvoted documents as
either interesting or not interesting to the user. The user may see the
documents classified as interesting him on his personal users page.

### Non-goals ###

Verditz is not a social network. Recommendations are generated only by using
machine learning systems.

The service is not optimized to handle a large group
of users. Even if it might be possible to support thousands of users with
recommendations, we will assume that a maximum of 10 users will use the service
at a time. This allows us to focus on finding the best possible learning
algorithms instead of excluding everything that needs a lot of computing power
from the start.


### Architecture ###

Verditz is made out of three larger modules:

A web application that serves as an user interface for both human
users as well as computer agents.

A crawling agent that fetched RSS feeds from a list of sources and stores
them in a database.

One or more machine learning agents that analyze the existing database
and create a list of recommendations for each registered user.

## Web Application ##

The user interface of Verditz will be implemented as a web application
that allows different users to register at the system.  All users will
have access to a global list of recent pages that is ordered by the
date of the submission, and may also submit new pages to this list.

Additionally to reading and adding to the recent pages list, users
will be able to place a vote to each page.  Votes will then be used as
a training set for the recommendation engine, which will search for
more articles in the page archive that the user (hopefully) also likes
to read.

Other than with a social bookmarking service such as reddit.com, the
votes will not be shared among users. This is important because
sharing votes (for example, by creating a list of the most upvoted
pages) would give the voting a ambiguous meaning, which in turn would
decrease the precision of the training data.

For example, consider that a user finds the topic of a particular
article interesting, but the user disagrees with the outcome. If
voting is shared among all registered users, the reader might be
tempted to downvote the article to 'protect' others from falling for
the false arguments.

Users may also hesitate to downvote articles that they know contains
valuable information, but are not interesting to the user at the time
of seeing it on reddit.

We believe that as soon as votings are shared, they automatically will
be used as a tool for communication.

The archive will save all pages and votings in the system for finite
amount of time. The best time should be as long as possible: We assume
that the information on most submitted pages will never be outdated,
so as more data we can store and use, the better.