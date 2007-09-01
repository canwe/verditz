package at.ac.tuwien.dbai.verditz.classify.test;

import junit.framework.TestCase;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import at.ac.tuwien.dbai.verditz.classify.NaiveBayesState;
import at.ac.tuwien.dbai.verditz.classify.TextClass;
import at.ac.tuwien.dbai.verditz.classify.TextClassifier;

public class TextClassifierTest extends TestCase {

	static {
		Logger.getRootLogger().setLevel(Level.DEBUG);
		Logger.getLogger(TextClassifier.class).setLevel(Level.DEBUG);
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
