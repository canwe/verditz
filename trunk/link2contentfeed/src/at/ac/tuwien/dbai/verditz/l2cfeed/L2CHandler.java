package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.mortbay.jetty.HttpConnection;
import org.mortbay.jetty.Request;
import org.mortbay.jetty.handler.AbstractHandler;

public class L2CHandler extends AbstractHandler {

	public void handle(String target, HttpServletRequest request,
			HttpServletResponse response, int dispatch) throws IOException,
			ServletException {
		Request base_request = (request instanceof Request) ? (Request) request
				: HttpConnection.getCurrentConnection().getRequest();
		base_request.setHandled(true);

		response.setContentType("text/html");
		response.setStatus(HttpServletResponse.SC_OK);
		response.getWriter().println("<h1>Hello L2C!</h1>");
	}

}