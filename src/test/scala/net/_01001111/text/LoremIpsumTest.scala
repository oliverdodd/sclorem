package net._01001111

import scala.util.Random
import scala.util.logging.ConsoleLogger
import org.junit._
import Assert._

class LoremIpsumTest extends Object with ConsoleLogger {
  
  val r = new Random
  val _n = System.getProperty("line.separator")
  
  def testString(s:String) = {
    log(s)
    assertNotNull(s)
	assertNotSame("String Not Empty", "", s)
  }

  def countChunks(s:String, d:String):Int = s.split(d).length
  def countWords(s:String):Int = countChunks(s, "[\\s]")
  def countSentences(s:String):Int = countChunks(s, "[.]")
  def countParagraphs(s:String):Int = countChunks(s, _n+_n)
  
  def randomint:Int = r.nextInt(20) + 1

  @Test
  def randomWord = testString(LoremIpsum.randomWord)
  
  @Test
  def randomPunctuation = testString(LoremIpsum.randomPunctuation)
  
  @Test
  def words = {
    val n = randomint
    val s = LoremIpsum.words(n)
    testString(s)
    val c = countWords(s)
    log("expecting " + n + " words, counted " + c)
    assertEquals(n, c)
  }

  @Test
  def sentenceFragment = {
    val m = 3
    val s = LoremIpsum.sentenceFragment
	testString(s)
	val c = countWords(s)
	log("expecting >= " + m + " words, counted " + c)
	assertTrue("Enough words", c >= m)
  }
  
  @Test
  def sentence = {
	val m = 3;
	val s = LoremIpsum.sentence
	testString(s)
	val c = countWords(s)
	log("expecting >= " + m + " words, counted " + c)
	assertTrue("Enough words", c >= m)
	assertTrue("First character uppercase", s.charAt(0) == s.toUpperCase.charAt(0))
	assertTrue("Is Punctuated", s.endsWith(".") || s.endsWith("?"))
  }
  
  @Test
  def sentences_count = {
	val n = randomint;
	val s = LoremIpsum.sentences(n);
	testString(s);
	val c = countSentences(s);
	log("expecting " + n + " sentences, counted " + c);
	assertEquals(n, c);
  }

  @Test
  def paragraph_useStandard = {
	val m = 2;
	val s = LoremIpsum.paragraph(true);
	testString(s);
	val c = countSentences(s);
	log("expecting > " + m + " sentences, counted " + c);
	assertTrue(c > m);
	assertTrue("Starts with standard", s
			.startsWith("Lorem ipsum dolor sit amet"));
  }

  @Test
  def paragraph = {
	val m = 2;
	val s = LoremIpsum.paragraph;
	testString(s);
	val c = countSentences(s);
	log("expecting >= " + m + " sentences, counted " + c);
	assertTrue("Enough sentences", c >= m);
  }

  @Test
  def paragraphs_count_useStandard = {
	val n = randomint;
	val s = LoremIpsum.paragraphs(n, true);
	testString(s);
	val c = countParagraphs(s);
	log("expecting " + n + " paragraphs, counted " + c);
	assertEquals(n, c);
	assertTrue("Starts with standard", s
			.startsWith("Lorem ipsum dolor sit amet"));
  }

  @Test
  def paragraphs_count = {
	val n = randomint;
	val s = LoremIpsum.paragraphs(n);
	testString(s);
	val c = countParagraphs(s);
	log("expecting " + n + " paragraphs, counted " + c);
	assertEquals(n, c);
  }
}