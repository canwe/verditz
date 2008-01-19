class UsersController < ApplicationController

  def login
    session[:user_id] = nil
    if request.post?
      user = User.authenticate(params[:name], params[:password])
      if user
        session[:user_id] = user.id
      else
        flash[:notice] = "Invalid user/password combination"
      end
      redirect_to(:back)
    end
  end

  def logout
    if request.post?
      session[:user_id] = nil
      redirect_to(:back)
    end
  end

  def register
    @user = User.new(params[:user])
    if request.post? and @user.save
      session[:user_id] = @user.id
      redirect_to(:action => "index", :controller => "articles")
    end
  end

  def recommendations
    user = User.find_by_name(params[:id])
    if session[:user_id].nil? or session[:user_id] != user.id
      render "shared/403", :status => 403
      return
    end
    @article_pages = Paginator.new self, user.recommended_articles.count, 30
    @articles = user.recommended_articles
    render "shared/list"
  end

  def votes
    user = User.find_by_name(params[:id])
    if session[:user_id].nil? or session[:user_id] != user.id
      render "shared/403", :status => 403
      return
    end
    @article_pages = Paginator.new self, user.voted_articles.count, 30
    @articles = user.voted_articles
    render "shared/list"
  end
end
