DROP TABLE IF EXISTS sources;
CREATE TABLE sources (
	id serial PRIMARY KEY,
	url varchar(1024) UNIQUE NOT NULL
);

DROP TABLE IF EXISTS articles;
CREATE TABLE articles (
	id serial PRIMARY KEY,
	title varchar(1024),
	text text,
	publish_time timestamp,
	url varchar(1024) NOT NULL,
	f_source integer REFERENCES sources
);
