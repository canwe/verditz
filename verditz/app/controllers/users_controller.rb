class UsersController < ApplicationController

  def login
    if request.post?
    end
  end

  def register
    @user = User.new(params[:user])
    if request.post? and @user.save
      flash.now[:notice] = "you've been successfully registered, #{@user.name}"
      @user = User.new
    end
  end
end
