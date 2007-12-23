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
    session[:user_id] = nil
    redirect_to(:back)
  end

  def register
    @user = User.new(params[:user])
    if request.post? and @user.save
      session[:user_id] = @user.id
      redirect_to(:action => "index", :controller => "articles")
    end
  end
end
