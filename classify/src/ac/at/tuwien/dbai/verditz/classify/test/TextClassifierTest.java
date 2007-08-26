package ac.at.tuwien.dbai.verditz.classify.test;

import java.util.HashMap;
import java.util.Map;

import junit.framework.TestCase;
import ac.at.tuwien.dbai.verditz.classify.TextClassifier;

public class TextClassifierTest extends TestCase {
 
	public TextClassifierTest(String name) {
		super(name);
	}

	private String capitalizeString(String s){
		s = s.toLowerCase();
		
		StringBuilder sb = new StringBuilder();
		sb.append(Character.toTitleCase(s.charAt(0)));
		sb.append(s.substring(1).toLowerCase());
		return sb.toString();
	}
	
	public void test() {

		final Map<String, String> properties = new HashMap<String, String>();
		properties.put("classifier.type",
				"at.ac.tuwien.dbai.verditz.classify.test.TestClassifier");

		final ClassifierFactory classifierFactory = new ClassifierFactory();
		final TextClassifier classifier = classifierFactory
				.createClassifier(properties);
		classifier.train("add this one to the sample set", TextClass.Hit);

		System.out.println(TextClass.valueOf("hit"));

		// classifier.trainMiss("add this one to the sample set as a miss");

		//TextClass cls = classifier.classify("tell me where I belong?");

		//assertEquals(cls == TextClass.Miss);

	}

	protected void setUp() throws Exception {
	}

	protected void tearDown() throws Exception {
	}

}
