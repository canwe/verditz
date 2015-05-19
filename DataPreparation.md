How to prepare the raw text content before classification/training.

## markup stripping ##

Because we are using RSS as the data source for our content, the body of a document will contain mostly the information we want. We can assume that RSS feeds do not contain as much as noise as, let's say, HTML pages. However, the content of an RSS usually contains some kind of markup (most often (X)HTML), links to other websites and media. Also, some authors have started to deliver advertisements with the feeds.

It would be easy to strip the markup from the content and only deal with the raw text. However, if the markup has a positive or negative effect on the results is still to be tested.

It is also most likely that different classification methods will react different to the markup.
