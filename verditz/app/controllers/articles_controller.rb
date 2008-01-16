class ArticlesController < ApplicationController
  def index
    list
    render :action => 'list'
  end

  def list
    @article_pages, @articles = paginate :articles, :per_page => 30, :order => "publish_time DESC"
  end

  def vote
    if request.post?
      article = Article.find(params[:id])
      user = User.find(session[:user_id])
      user.vote(article, params[:vote].to_i)
      @article = article
    end
  end

  def bookmarklet_vote
    vote
  end

  def new
    if request.post?
      @id = Article.index(params[:url])
      render :action => "new", :layout => false
    end
  end

  def bookmarklet_js
    response.headers['Content-Type'] = 'text/javascript'
    render :action => "bookmarklet_js", :layout => false
  end

  def bookmarklet_iframe
  end
end
