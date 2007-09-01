package at.ac.tuwien.dbai.verditz.classify;

import java.util.Set;

import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.Instances;
import weka.filters.Filter;

public interface ClassifierState {
	public Classifier getClassifier();

	public Set<Filter> getFilterSet();

	public Instances getInstances();

}
