require "csv"
require "rexml/document"
require "rexml/cdata"
require "rexml/xmldecl"



#"id","title","body","source","Ben Read","Ben Titles","Michi Read","Michi Titles","Thomas Mixed"

doc = REXML::Document.new "", { :raw => :all }
items = doc.add_element "items"

samples = {4 => "ben", 5 => "ben_title", 6 => "michi", 7 => "michi_title", 8 => "thomas_mixed" }

CSV::Reader.parse(File.open('data.csv', 'rb')) do |row|
  item = items.add_element "item"
  votes = item.add_element "votes"


  samples.each{|k,v|
    if(row[k] != "No Vote") then 
      vote = votes.add_element "vote", {"sample" => v}
      vote.add_text row[k] 
    end    
  }


  link = item.add_element "link"
  REXML::CData.new( "#{row[3]}", true, link )

  title = item.add_element "title"
  REXML::CData.new( "#{row[1]}", true, title )

  body = item.add_element "body"
  REXML::CData.new( "#{row[2]}", true, body )
  
end

#doc << XmlDecl.new
doc.write File.new("samples.xml","w"), 0
