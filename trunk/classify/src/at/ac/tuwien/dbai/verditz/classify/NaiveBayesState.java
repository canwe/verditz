package at.ac.tuwien.dbai.verditz.classify;

import java.util.HashSet;
import java.util.Set;


import weka.classifiers.Classifier;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instances;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.StringToWordVector;

public final class NaiveBayesState implements ClassifierState {

	private final static String DATASET_NAME = "VerditzTextClassification";
	private final static String ATTRIBUTE_NAME = "Message";

	private final NaiveBayes classifier;
	private final Instances instances;

	public NaiveBayesState() {
		this.classifier = new NaiveBayes();
		this.instances = this.createInstances();
	}

	@Override
	public Classifier getClassifier() {
		return this.classifier;
	}

	@Override
	public Set<Filter> getFilterSet() {
		Set<Filter> filterSet = new HashSet<Filter>();
		filterSet.add(new StringToWordVector());
		return filterSet;
	}

	@Override
	public Instances getInstances() {
		return this.instances;
	}

	private Instances createInstances() {
		// Create vector of attributes.
		FastVector attributes = new FastVector(2);
		// Add attribute for holding messages.
		attributes.addElement(new Attribute(ATTRIBUTE_NAME, (FastVector) null));
		// Add class attribute.
		FastVector classValues = new FastVector(2);
		classValues.addElement(TextClass.Hit.toString());
		classValues.addElement(TextClass.Miss.toString());
		attributes.addElement(new Attribute(TextClass.Class.toString(),
				classValues));
		Instances instances = new Instances(DATASET_NAME, attributes, 100);
		instances.setClassIndex(instances.numAttributes() - 1);
		return instances;
	}


}
