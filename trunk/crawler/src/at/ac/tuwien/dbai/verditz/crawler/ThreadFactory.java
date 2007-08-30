package at.ac.tuwien.dbai.verditz.crawler;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;

import org.apache.log4j.Logger;

public class ThreadFactory implements java.util.concurrent.ThreadFactory {

	private static final Logger log = Logger.getLogger(ThreadFactory.class);

	private Collection<Thread> threads = new ArrayList<Thread>();

	private final Integer maxThreads;

	public ThreadFactory(final Integer max) {
		this.maxThreads = max;
	}

	public Thread newThread(Runnable runnable) {
		removeDeadThreads();
		while (this.getThreads().size() >= this.maxThreads) {
			try {
				Thread.sleep(5);
				removeDeadThreads();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
		}

		Thread thread = new Thread(runnable);
		threads.add(thread);
		log.debug("active threads:" + threads.size());
		return thread;
	}

	public Collection<Thread> getThreads() {
		return Collections.unmodifiableCollection(this.threads);
	}

	public void removeDeadThreads() {
		Collection<Thread> livingThreads = new ArrayList<Thread>();
		for (Thread thread : this.threads) {
			if (thread.isAlive()) {
				livingThreads.add(thread);
			}
		}
		this.threads = livingThreads;
	}

}
