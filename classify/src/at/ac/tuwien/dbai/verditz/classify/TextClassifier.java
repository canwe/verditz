package at.ac.tuwien.dbai.verditz.classify;

import java.util.Collection;

import org.apache.log4j.Logger;

import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.Filter;

public final class TextClassifier {

	private static final Logger log = Logger.getLogger(TextClassifier.class);

	private ClassifierState state;

	/* Whether the model is up to date. */
	public TextClassifier(final ClassifierState state) {
		this.state = state;
	}

	public void train(final String message, final TextClass cls)
			throws Exception {
		log.info("train: " + cls);
		this.updateData(message, cls.toString());
		updateClassifier();
	}

	public void trainAll(final Collection<String> messages, final TextClass cls)
			throws Exception {
		for (String message : messages) {
			log.info("train: " + cls);
			this.updateData(message, cls.toString());
		}
		updateClassifier();
	}

	public TextClass classify(final String message) throws Exception {
		log.debug("starting classification");
		final long t1 = System.currentTimeMillis();
		final Instances instances = state.getInstances();
		final Classifier classifier = state.getClassifier();
		final Filter filter = state.getFilter();

		// Make separate little test set so that message
		// does not get added to string attribute in m_Data.
		final Instances testset = instances.stringFreeStructure();
		// Make message into test instance.
		final Instance instance = makeInstance(message, testset);

		filter.input(instance);
		filter.batchFinished();
		Instance filteredInstance = filter.output();

		// Get index of predicted class value.
		final double predicted = state.getClassifier().classifyInstance(
				filteredInstance);

		double time = (System.currentTimeMillis() - t1) / 60.0;
		log.debug("finfished classification. needed " + time + " seconds");

		return TextClass.valueOf(this.state.getInstances().classAttribute()
				.value((int) predicted));

	}

	/**
	 * Updates data using the given training message.
	 * 
	 * @throws Exception
	 */
	private void updateData(final String message, final String classValue) {
		// Make message into instance.
		final Instance instance = makeInstance(message, this.state
				.getInstances());
		// Set class value for instance.
		instance.setClassValue(classValue);
		// Add instance to training data.
		this.state.getInstances().add(instance);
	}

	private void updateClassifier() throws Exception {
		Filter filter = state.getFilter();
		Instances instances = state.getInstances();
		filter.setInputFormat(instances);
		state.getClassifier().buildClassifier(
				Filter.useFilter(instances, filter));
	}

	/**
	 * Method that converts a text message into an instance.
	 */
	private Instance makeInstance(final String text, final Instances instances) {

		// Create instance of length two.
		Instance instance = new Instance(2);
		Attribute attribute = instances.attribute("Message");
		instance.setValue(attribute, attribute.addStringValue(text));
		// Give instance access to attribute information from the dataset.
		instance.setDataset(instances);
		return instance;
	}

	public int numInstances() {
		return this.state.getInstances().numInstances();
	}

}
