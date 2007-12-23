module ArticlesHelper
  def up_vote_path article
    return "images/up_off.gif" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "images/up_off.gif" if vote.nil? or vote.value <= 0
    return "images/up_on.gif"
  end

  def down_vote_path article
    return "images/down_off.gif" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "images/down_off.gif" if vote.nil? or vote.value >= 0
    return "images/down_on.gif"
  end

  def up_vote_value article
    return "1" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "1" if vote.nil? or vote.value <= 0
    return "0"
  end

  def down_vote_value article
    return "-1" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "-1" if vote.nil? or vote.value >= 0
    return "0"
  end
end
