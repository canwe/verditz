package at.ac.tuwien.dbai.verditz.classify.test;

import java.io.IOException;
import java.util.List;

import javax.xml.parsers.ParserConfigurationException;
import javax.xml.xpath.XPathExpressionException;

import junit.framework.TestCase;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.xml.sax.SAXException;

import at.ac.tuwien.dbai.verditz.classify.NaiveBayesState;
import at.ac.tuwien.dbai.verditz.classify.TextClass;
import at.ac.tuwien.dbai.verditz.classify.TextClassifier;

public class TextClassifierTest extends TestCase {

	static {
		Logger.getRootLogger().setLevel(Level.ERROR);
		Logger.getLogger(TextClassifier.class).setLevel(Level.ERROR);
		Logger.getLogger(SampleCorpus.class).setLevel(Level.ERROR);
	}

	public TextClassifierTest(final String name) {
		super(name);
	}

	private String capitalizeString(String s) {
		s = s.toLowerCase();

		StringBuilder sb = new StringBuilder();
		sb.append(Character.toTitleCase(s.charAt(0)));
		sb.append(s.substring(1).toLowerCase());
		return sb.toString();
	}

	public void testTrain() {
		final TextClassifier classifier = new TextClassifier(
				new NaiveBayesState());

		classifier.train("add this one to the sample set", TextClass.Hit);

		classifier.train("add this one to the sample set as a miss",
				TextClass.Miss);

		assertEquals(2, classifier.numInstances());

	}

	public void testTrainAndClassifyMany() throws XPathExpressionException, SAXException, IOException, ParserConfigurationException{
		SampleCorpus corpus = new SampleCorpus(new java.io.File(
		"data/samples.xml"), 50);
		
		for(int i=0;i<50;i++){
			trainAndClassify(corpus);
			corpus.shuffle();
			System.out.println("----------------------------\n\n");
		}
			
	}
	
	private void trainAndClassify(SampleCorpus corpus) throws SAXException, IOException,
			ParserConfigurationException, XPathExpressionException {
		final TextClassifier classifier = new TextClassifier(
				new NaiveBayesState());

		
		for (String sample : corpus.getSamples()) {
			System.out.println("Sample: " + sample);

			// train
			for (String message : corpus.getPositiveTrainingData(sample)) {
				classifier.train(message, TextClass.Hit);
			}
			
			System.out.println("trained " + corpus.getPositiveTrainingData(sample).size() + " positives" );
			
			for (String message : corpus.getNegativeTrainingData(sample)) {
				classifier.train(message, TextClass.Miss);
			}

			System.out.println("trained " + corpus.getNegativeTrainingData(sample).size() + " negatives" );

			
			int matchesPositive = 0;
			int matchesNegative = 0;

			for (String message : corpus.getPositiveTestData(sample)) {

				if (classifier.classify(message) == TextClass.Hit) {
					matchesPositive++;
				} 

			}

			for (String message : corpus.getNegativeTestData(sample)) {
				if (classifier.classify(message) == TextClass.Miss) {
					matchesNegative++;
				}
			}

			System.out.println("correct positive matches: " + matchesPositive
					+ "(from " + corpus.getPositiveTestData(sample).size() + ")");

			System.out.println("correct negative matches: " + matchesNegative + "(from "
					+ corpus.getNegativeTestData(sample).size() + ")");

		}

	}

	public void testTrainAndClassifySimple() throws Exception {

		final TextClassifier classifier = new TextClassifier(
				new NaiveBayesState());

		classifier
				.train(
						"A Ruby I dig, A Python I fight, I trink too much Java and code C into the night.",
						TextClass.Hit);

		classifier.train("string with the useless junk, yoda said!",
				TextClass.Miss);

		TextClass missCls = classifier.classify("junk");
		assertEquals(TextClass.Miss, missCls);

		TextClass hitCls = classifier.classify("A Ruby I dig, and Java");
		assertEquals(TextClass.Hit, hitCls);

	}

	protected void setUp() throws Exception {
	}

	protected void tearDown() throws Exception {
	}

}
