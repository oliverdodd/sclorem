sclorem
=======
A Scala Lorem Ipsum Generator.


Usage
-----
	val sclorem = new LoremIpsum
	val paragraphs = sclorem.paragraphs(3, true)
	
###Output:

	Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.

	Sapien gravida arcu integer justo mauris nam ante.  Mus torquent sollicitudin dictum leo et per scelerisque cursus cras.  Mus tempor dapibus facilisis id venenatis iaculis sed faucibus lectus dui primis?  Risus est praesent eget consectetur tristique phasellus, natoque luctus nam potenti quam consectetur blandit?

	Erat eleifend dictumst mollis pharetra inceptos, odio dictum arcu quisque mauris feugiat, quam leo imperdiet lacus sociosqu venenatis turpis integer?  Pellentesque mattis adipiscing auctor tincidunt enim, augue sagittis viverra luctus at nulla, conubia id dictum ligula tellus urna tristique erat hac purus, et cum tristique ut per, cum dictumst platea egestas augue varius molestie curabitur proin mauris?  Sed ut mi lorem in inceptos interdum.  Pellentesque amet dis rutrum vivamus taciti libero convallis ante cum habitasse conubia class, eu sociis interdum ullamcorper odio morbi dui natoque bibendum placerat vestibulum luctus.  Eros faucibus adipiscing turpis massa sem tincidunt iaculis est ad lacus ipsum iaculis?
	
Methods
-------

+	**randomWord:String**
	- Get a random word.
+	**randomPunctuation:String**
	- Get a random punctuation mark.
+	**words(count:Int):String**
	- Get a string of words.
+	**sentenceFragment:String**
	- Get a sentence fragment.
+	**sentence:String**
	- Get a sentence.
+	**sentences(count:Int):String**
	- Get multiple sentences.
+	**paragraph(useStandard:Boolean = false)**
	- Get a paragraph.
	If useStandard is true, return the standard Lorem Ipsum paragraph.
+	**paragraphs(count: Int, useStandard:Boolean = false)**
	- Get multiple paragraphs.
	If useStandard is true, lead with the standard Lorem Ipsum paragraph.