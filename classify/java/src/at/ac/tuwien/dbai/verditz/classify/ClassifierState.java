package at.ac.tuwien.dbai.verditz.classify;

import java.util.Set;

import weka.classifiers.Classifier;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.Filter;

public interface ClassifierState {
	public Classifier getClassifier();

	Filter getFilter();

	Instances getInstances();

	/**
	 * Method that converts a text message into an instance.
	 */
	Instance makeInstance(final String text, final Instances instances);

}
