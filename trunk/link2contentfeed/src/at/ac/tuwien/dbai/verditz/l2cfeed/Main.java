package at.ac.tuwien.dbai.verditz.l2cfeed;

import java.io.FileInputStream;

import org.mortbay.jetty.Server;
import org.mortbay.xml.XmlConfiguration;

public class Main {

	public void start() throws Exception {
		Server server = new Server();
		XmlConfiguration configuration = new XmlConfiguration(
				new FileInputStream("config/jetty.xml"));
		configuration.configure(server);
		server.start();
		server.join();
	}

	public static void main(String[] args) {
		try {
			new Main().start();
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}