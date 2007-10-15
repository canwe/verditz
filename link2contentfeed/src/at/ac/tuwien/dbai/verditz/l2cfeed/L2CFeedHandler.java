package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.io.IOException;
import java.net.MalformedURLException;
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
import at.ac.tuwien.dbai.verditz.crawler.FilePersistentCache;

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
				} else {
					SyndFeed feed;
					try {
						feed = new FilePersistentCache().getFeedInfo(new URL(event.getUrlString())).getSyndFeed();
						feeds.add(L2CFeed.link2Content(feed));
					} catch (MalformedURLException e) {
						log.error(e);
					}
				}
			}

		};
		FeedCrawler crawler = new FeedCrawler(handler);

		List<URL> feed = new ArrayList<URL>();

		URL url = new URL(request.getParameter("feed"));
		feed.add(url);
		crawler.addFeedSource(feed);
		crawler.fetch();

		response.setContentType("application/rss+xml");
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