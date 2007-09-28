package at.ac.tuwien.dbai.verditz.classify.test;

import java.io.File;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.xpath.XPath;
import javax.xml.xpath.XPathConstants;
import javax.xml.xpath.XPathFactory;

import org.apache.log4j.Logger;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;

public class SampleCorpus {

	private static Logger log = Logger.getLogger(SampleCorpus.class);

	private final Document document;
	final XPath xpath = XPathFactory.newInstance().newXPath();
	private final int percentTraining;
	private final Map<String, List<String>> positivesMap = new HashMap<String, List<String>>();
	private final Map<String, List<String>> negativesMap = new HashMap<String, List<String>>();

	private final static String ATTRIBUTE_TO_TEST = "body";

	public SampleCorpus(final File xmlFile, int percentTraining) {
		try {
			this.document = DocumentBuilderFactory.newInstance()
					.newDocumentBuilder().parse(
							new java.io.File("data/samples.xml"));

			for (final String sample : getSamples()) {
				this.positivesMap.put(sample, findPositives(sample));
				this.negativesMap.put(sample, findNegatives(sample));
			}

			this.percentTraining = percentTraining;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public Collection<String> getSamples() {
		Collection<String> samples = new LinkedList<String>();
		try {
			NodeList sampleNodes = (NodeList) xpath.evaluate(
					"/corpus/samples/sample/text()", document,
					XPathConstants.NODESET);
			for (int i = 0; i < sampleNodes.getLength(); i++) {
				samples.add(((Node) sampleNodes.item(i)).getNodeValue());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return Collections.unmodifiableCollection(samples);
	}

	public void shuffle() {
		for (List<String> list : this.positivesMap.values()) {
			Collections.shuffle(list);
		}

		for (List<String> list : this.negativesMap.values()) {
			Collections.shuffle(list);
		}
	}

	private int middleIndex(List l) {
		double f = (this.percentTraining / 100.0f) * l.size();
		int n = (int) Math.round(f);
		log.debug("split samples at element nr. = " + n);
		return n;
	}

	public List<String> getPositiveTrainingData(String sample) {
		List<String> samples = getPositives(sample);
		return samples.subList(0, middleIndex(samples));
	}

	public List<String> getPositiveTestData(String sample) {
		List<String> samples = getPositives(sample);
		return samples.subList(middleIndex(samples) + 1, samples.size() - 1);
	}

	public List<String> getNegativeTrainingData(String sample) {
		List<String> samples = getNegatives(sample);
		return samples.subList(0, middleIndex(samples));
	}

	public List<String> getNegativeTestData(String sample) {
		List<String> samples = getNegatives(sample);
		return samples.subList(middleIndex(samples) + 1, samples.size() - 1);
	}

	private List<String> getPositives(String sample) {
		if (positivesMap.get(sample).isEmpty()) {
			throw new IllegalArgumentException(
					"no positive data found for this sample: " + sample);
		}
		return this.positivesMap.get(sample);
	}

	private List<String> getNegatives(String sample) {
		if (negativesMap.get(sample).isEmpty()) {
			throw new IllegalArgumentException(
					"no negative data found for this sample: " + sample);
		}
		return this.negativesMap.get(sample);
	}

	private List<String> findPositives(String sample) {
		List<String> samples = new LinkedList<String>();
		try {
			final NodeList sampleNodes = (NodeList) xpath.evaluate(
					"/corpus/item[votes/vote[@sample = '" + sample
							+ "']/text() = 'Up']/" + ATTRIBUTE_TO_TEST
							+ "/text()", document, XPathConstants.NODESET);
			for (int i = 0; i < sampleNodes.getLength(); i++) {
				samples.add(((Node) sampleNodes.item(i)).getNodeValue());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return samples;
	}

	private List<String> findNegatives(String sample) {
		List<String> samples = new LinkedList<String>();
		try {
			final NodeList sampleNodes = (NodeList) xpath.evaluate(
					"/corpus/item[votes/vote[@sample = '" + sample
							+ "']/text() = 'Down']/" + ATTRIBUTE_TO_TEST
							+ "/text()", document, XPathConstants.NODESET);
			for (int i = 0; i < sampleNodes.getLength(); i++) {
				samples.add(((Node) sampleNodes.item(i)).getNodeValue());
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return samples;
	}

}
