package at.ac.tuwien.dbai.verditz.classify.test;

import junit.framework.TestCase;

import org.apache.log4j.Level;
import org.apache.log4j.Logger;

import weka.classifiers.Classifier;
import weka.classifiers.bayes.NaiveBayes;
import at.ac.tuwien.dbai.verditz.classify.TextClass;
import at.ac.tuwien.dbai.verditz.classify.TextClassifier;
import at.ac.tuwien.dbai.verditz.classify.WordState;

public class TextClassifierTest extends TestCase {

	private static Logger log = Logger.getLogger(TextClassifierTest.class);

	static {
		Logger.getRootLogger().setLevel(Level.ERROR);
		Logger.getLogger(TextClassifierTest.class).setLevel(Level.DEBUG);
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

	public void _testTrain() throws Exception {
		final TextClassifier classifier = new TextClassifier(new WordState(
				new NaiveBayes()));

		classifier.train("add this one to the sample set", TextClass.Hit);

		classifier.train("add this one to the sample set as a miss",
				TextClass.Miss);

		assertEquals(2, classifier.numInstances());

	}

	public void testTrainAndClassifyMany() throws Exception {
		SampleCorpus corpus = new SampleCorpus(new java.io.File(
				"data/samples.xml"), 75);

		for (int i = 0; i < 5; i++) {
			trainAndClassify(corpus);
			corpus.shuffle();
			log.info("----------------------------\n\n");
		}

	}

	private void trainAndClassify(SampleCorpus corpus) throws Exception {

		
		Classifier cls = new weka.classifiers.trees.J48();
		
		for (String sample : corpus.getSamples()) {
			System.out.println("Sample: " + sample);
			final TextClassifier classifier = new TextClassifier(new WordState(
					cls));
			// train
			classifier.trainAll(corpus.getPositiveTrainingData(sample),
					TextClass.Hit);

			log.info("trained " + corpus.getPositiveTrainingData(sample).size()
					+ " positives");

			classifier.trainAll(corpus.getNegativeTrainingData(sample),
					TextClass.Miss);

			log.info("trained " + corpus.getNegativeTrainingData(sample).size()
					+ " negatives");

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

			log.info("correct positive matches: " + matchesPositive + "(from "
					+ corpus.getPositiveTestData(sample).size() + ")");

			log.info("correct negative matches: " + matchesNegative + "(from "
					+ corpus.getNegativeTestData(sample).size() + ")");

		}

	}

	public void _testTrainAndClassifySimple() throws Exception {

		final TextClassifier classifier = new TextClassifier(new WordState(
				new weka.classifiers.rules.OneR()));

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
