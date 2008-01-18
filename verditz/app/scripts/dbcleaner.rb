require "../../config/environment"
require "net/http"
require "uri"

articles = Article.find(:all)
articles.each { |article|
  begin
    uri = URI.parse(article.url)
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
  rescue URI:InvalidURIError
    p "#{article.url}: invalid URI. deleting article."
  rescue SocketError
    p "#{article.url}: host not reachable. deleting article."
    article.destroy
  rescue Exception
    p "#{article.url}: #{$!}. deleting article."
    article.destroy
  end
}
