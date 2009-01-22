# Methods added to this helper will be available to all templates in the application.
module ApplicationHelper
  def up_vote_path article
    return "/images/up_off.gif" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "/images/up_off.gif" if vote.nil? or vote.value <= 0
    return "/images/up_on.gif"
  end

  def down_vote_path article
    return "/images/down_off.gif" unless session[:user_id]
    vote = Vote.find(:first, :conditions => ["user_id = ? and article_id = ?", session[:user_id], article.id])
    return "/images/down_off.gif" if vote.nil? or vote.value >= 0
    return "/images/down_on.gif"
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

  def render_if_logged_in
    unless session[:user_id].nil?
      yield
    end
  end

  def render_if_logged_out
    if session[:user_id].nil?
      yield
    end
  end

  def username
    return "anonymous" if session[:user_id].nil?
    return User.find(session[:user_id]).name
  end
  
  def timeago(time, options = {})
    start_date = options.delete(:start_date) || Time.new
    date_format = options.delete(:date_format) || :default
    delta_minutes = (start_date.to_i - time.to_i).floor / 60
    if delta_minutes.abs <= (8724*60) # eight weeks… I’m lazy to count days for longer than that
      distance = distance_of_time_in_words(delta_minutes);
      if delta_minutes < 0
        "#{distance} from now"
      else
        "#{distance} ago"
      end
    else
      return time.strftime("%B %d, %Y at %H:%M")
    end
  end

  def distance_of_time_in_words(minutes)
    case
    when minutes < 1
      "less than a minute"
    when minutes < 50
      pluralize(minutes, "minute")
    when minutes < 90
      "about one hour"
    when minutes < 1080
      "#{(minutes.to_f / 60.to_f).round} hours"
    when minutes < 1440
      "one day"
    when minutes < 2880
      "about one day"
    else
      "#{(minutes / 1440).round} days"
    end
  end
end
