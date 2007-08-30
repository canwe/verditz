--gonna do a lot of changes to the schema 
drop table if exists documents;

--start with the most basic fields we need for text processing
create table documents(
	id int(11) AUTO_INCREMENT,
	title varchar(255),
	body text,	
	source varchar(255),
	primary key (id)
);