package ac.at.tuwien.dbai.verditz.classify.test;

import java.util.Map;

import ac.at.tuwien.dbai.verditz.classify.TextClassifier;

public class ClassifierFactory {
	public TextClassifier createClassifier(Map<String, String> properties) {
		return new TextClassifier();
	}
}
