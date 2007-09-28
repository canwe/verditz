package at.ac.tuwien.dbai.verditz.classify;

import weka.classifiers.Classifier;
import weka.classifiers.bayes.NaiveBayes;
import weka.core.Instance;
import weka.core.Instances;
import weka.filters.Filter;

public class NaiveBayesOnWordsState implements ClassifierState {

	ClassifierState state = new WordState(new NaiveBayes());
	
	@Override
	public Classifier getClassifier() {
		return state.getClassifier();
	}

	@Override
	public Filter getFilter() {
		return state.getFilter();
	}

	@Override
	public Instances getInstances() {
		return state.getInstances();
	}

	@Override
	public Instance makeInstance(String text, Instances instances) {
		return state.makeInstance(text, instances);
	}

}
