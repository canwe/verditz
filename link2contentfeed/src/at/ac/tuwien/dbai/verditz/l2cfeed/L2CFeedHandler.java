package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.log4j.Logger;
import org.mortbay.jetty.HttpConnection;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.handler.AbstractHandler;

import at.ac.tuwien.dbai.verditz.crawler.FeedCrawler;

import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.fetcher.FetcherEvent;
import com.sun.syndication.fetcher.FetcherListener;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.SyndFeedOutput;

public class L2CFeedHandler extends AbstractHandler {

	private final static Logger log = Logger.getLogger(L2CFeedHandler.class);

	public void handle(String target, HttpServletRequest request,
			HttpServletResponse response, int dispatch) throws IOException,
			ServletException {
		Request base_request = (request instanceof Request) ? (Request) request
				: HttpConnection.getCurrentConnection().getRequest();
		base_request.setHandled(true);

		final List<SyndFeed> feeds = new ArrayList<SyndFeed>();

		final FetcherListener handler = new FetcherListener() {

			@SuppressWarnings("unchecked")
			public void fetcherEvent(FetcherEvent event) {
				if (event.getEventType() == FetcherEvent.EVENT_TYPE_FEED_RETRIEVED) {
					SyndFeed feed = event.getFeed();
					feeds.add(L2CFeed.link2Content(feed));
				}
			}

		};
		FeedCrawler crawler = new FeedCrawler(handler);

		URL url = new URL(request.getParameter("feed"));
		crawler.addSource(url);
		crawler.fetch();

		response.setContentType("application/rss+xml");
		response.setCharacterEncoding("UTF-8");
		response.setStatus(HttpServletResponse.SC_OK);
		SyndFeedOutput feedOutput = new SyndFeedOutput();
		try {
			String xmlFeed = feedOutput.outputString(feeds.get(0));
			response.getWriter().print(xmlFeed);
		} catch (FeedException e) {
			e.printStackTrace();
		}
	}
}