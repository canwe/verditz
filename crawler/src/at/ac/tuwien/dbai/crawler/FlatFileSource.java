package at.ac.tuwien.dbai.crawler;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.apache.log4j.Logger;


public class FlatFileSource implements Iterable<URL> {

    private final Collection<URL> urls = new ArrayList<URL>();
    private final Logger log = Logger.getLogger(getClass());

    @SuppressWarnings("unchecked")
    public FlatFileSource(File file) {
        try {
            final List<String> lines = FileUtils.readLines(file, "UTF-8");
            for (String line : lines) {
                urls.add(new URL(line));
            }
        } catch (IOException e) {
            log.error(e.getMessage(), e);
        }
    }

    public Iterator<URL> iterator() {
        return urls.iterator();
    }

}
