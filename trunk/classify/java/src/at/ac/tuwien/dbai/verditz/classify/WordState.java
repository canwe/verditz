package at.ac.tuwien.dbai.verditz.classify;

import weka.classifiers.Classifier;
import weka.core.Attribute;
import weka.core.FastVector;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.Filter;
import weka.filters.unsupervised.attribute.StringToWordVector;

public final class WordState implements ClassifierState {

	private final static String DATASET_NAME = "VerditzTextClassification";
	private final static String ATTRIBUTE_NAME = "Message";

	private final Classifier classifier;
	private final Instances instances;

	private final Filter filter;

	public WordState(final Classifier classifier) {
		this.classifier = classifier;
		this.instances = this.createInstances();
		this.filter = new StringToWordVector();
	}

	@Override
	public Classifier getClassifier() {
		return this.classifier;
	}

	@Override
	public Filter getFilter() {
		return filter;
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

	@Override
	public Instance makeInstance(final String text, final Instances instances) {
		// Create instance of length two.
		Instance instance = new Instance(2);
		Attribute attribute = instances.attribute(ATTRIBUTE_NAME);
		instance.setValue(attribute, attribute.addStringValue(text));
		// Give instance access to attribute information from the dataset.
		instance.setDataset(instances);
		return instance;
	}

}
