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
end
