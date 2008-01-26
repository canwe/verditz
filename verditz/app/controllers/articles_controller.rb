class ArticlesController < ApplicationController
  def index
    list
    respond_to do |format|
      format.html {
        @feeds = ["#{url_for(:controller => "articles", :action => "index", :format => "xml")}"]
        render "shared/list"
      }
      format.xml {
        @title = "recently indexed"
        render_without_layout "shared/list.rxml"
      }
    end
  end

  def list
    @article_pages, @articles = paginate :articles, :per_page => 30, :order => "publish_time DESC"
  end

  def vote
    if request.post?
      if session[:user_id].nil?
        render :action => "vote", :status => 401
        return
      end
      article = Article.find(params[:id])
      user = User.find(session[:user_id])
      user.vote(article, params[:vote].to_i)
      @article = article
    end
  end

  def bookmarklet_vote
    vote
    render :layout => false
  end

  def new
    if request.post?
      if session[:user_id].nil?
        render :action => "new", :status => 401
        return
      end
      @id = Article.index(params[:url])
      render :action => "new", :layout => false
    end
  end

  def bookmarklet_js
    response.headers['Content-Type'] = 'text/javascript'
    render :action => "bookmarklet_js", :layout => false
  end

  def bookmarklet_iframe
    render :layout => "bookmarklet"
  end
end
