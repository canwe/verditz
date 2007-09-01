package at.ac.tuwien.dbai.verditz.classify;

import org.apache.log4j.Logger;

import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.Filter;
import at.ac.tuwien.dbai.verditz.classify.test.ClassifierState;
import at.ac.tuwien.dbai.verditz.classify.test.TextClass;

public final class TextClassifier {

	private static final Logger log = Logger.getLogger(TextClassifier.class);

	private ClassifierState state;

	public TextClassifier(final ClassifierState state) {
		this.state = state;
	}

	public void train(final String message, final TextClass cls) {
		log.info("train: " + cls);
		this.updateData(message, cls.toString());
	}

	public TextClass classify(final String message) {
		try {

			Instances instances = this.state.getInstances();
			Classifier classifier = this.state.getClassifier();

			// Make separate little test set so that message
			// does not get added to string attribute in m_Data.
			final Instances testset = instances.stringFreeStructure();
			// Make message into test instance.
			final Instance instance = makeInstance(message, testset);

			Instance filteredInstance = instance;
			for (Filter filter : this.state.getFilterSet()) {
				log.debug("filter.isOutputFormatDefined()"
						+ filter.isOutputFormatDefined());
				filter.setInputFormat(instances);
				classifier.buildClassifier(Filter.useFilter(instances, filter));
				filter.input(filteredInstance);

				filter.batchFinished();
				filteredInstance = filter.output();
			}
			// Get index of predicted class value.
			final double predicted = this.state.getClassifier()
					.classifyInstance(filteredInstance);

			return TextClass.valueOf(this.state.getInstances().classAttribute()
					.value((int) predicted));
		} catch (Exception e) {
			log.error(e);
			throw new RuntimeException(e);
		}
	}

	/**
	 * Updates data using the given training message.
	 */
	private void updateData(final String message, final String classValue) {
		// Make message into instance.
		final Instance instance = makeInstance(message, this.state
				.getInstances());
		// Set class value for instance.
		instance.setClassValue(classValue);
		// Add instance to training data.
		this.state.getInstances().add(instance);
		// filterInstances();
	}

	private void filterInstances() {
		final Classifier classifier = this.state.getClassifier();
		final Instances instances = state.getInstances();

		for (Filter filter : state.getFilterSet()) {
			try {
				log.debug("filterInstances -- filter.isOutputFormatDefined()"
						+ filter.isOutputFormatDefined());
				filter.setInputFormat(instances);
				classifier.buildClassifier(Filter.useFilter(instances, filter));
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}

	/**
	 * Method that converts a text message into an instance.
	 */
	private Instance makeInstance(final String text, final Instances instances) {

		// Create instance of length two.
		Instance instance = new Instance(2);
		// Set value for message attribute
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
