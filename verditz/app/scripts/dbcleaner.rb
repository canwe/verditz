require "../../config/environment"
require "net/http"
require "uri"

articles = Article.find(:all)
articles.each { |article|
  uri = URI.parse(article.url)
  begin
    Net::HTTP.start(uri.host, uri.port) { |http|
      response = http.request_head(uri.request_uri, initheader={'User-Agent','Mozilla/5.0'})
      if response.kind_of? Net::HTTPRedirection
        p "#{article.url} => #{response['location']}"
        article.url = response['location']
        article.save
      elsif response.kind_of? Net::HTTPClientError
        p "#{article.url}: #{response.class}. deleting article."
        article.destroy
      end
    }
  rescue SocketError
    p "#{article.url}: host not reachable. deleting article."
    article.destroy
  end
}
