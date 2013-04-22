package gpp.classify

import nak.data._

import chalk.lang.eng.PorterStemmer

import gpp.util.{English, SimpleTokenizer}

/**
 * A trait for classes that can extract features from the information in
 * the tweets.
 */
trait FeatureExtractor {
  
  /**
   * Given the content of a tweet, extract a set of features
   * and their corresponding value.
   */
  def apply(item: String): Seq[FeatureObservation[String]]
}

/**
 * The simplest feature extractor: each word gets a feature 
 */
object BasicFeatureExtractor extends FeatureExtractor {

  override def apply(item: String): Seq[FeatureObservation[String]] = {
    SimpleTokenizer(item).map(descr=>FeatureObservation(descr))
  }

}

/**
 * An extended feature extractor.
 */
object ExtendedFeatureExtractor extends FeatureExtractor {

  lazy val stemmer = new PorterStemmer
	lazy val posWords = English.getLexicon("positive-words.txt.gz")
	lazy val negWords = English.getLexicon("negative-words.txt.gz")
	lazy val posEmots = Array(":)", ":D", "=D", "=)", ":]", "=]", ":-)", ":-D", ":-]", ";)", ";D", ";]", ";-)", ";-D", ";-]", "^_^")
	lazy val negEmots = Array(":(", "=(", ":[", "=[", ":-(", ":-[", ":’(", ":’[", "D:", "/:")

  override def apply(item: String): Seq[FeatureObservation[String]] = {

	val polarity = getPolarity(item)
	val emotion = getEmoticonPolarity(item)

	val tokens = SimpleTokenizer(item).map(_.toLowerCase)

    	val wordFeatures = tokens.filterNot(English.stopwords)
			.map(stemmer(_))
			
	val bigrams = tokens.sliding(2).map(_.mkString(" "))

	val trigrams = tokens.sliding(3).map(_.mkString(" "))

	val features = wordFeatures ++ bigrams ++ trigrams ++ Seq[String](polarity, emotion) 

	features.map(descr=>FeatureObservation(descr))
  }

	def getPolarity(text: String): String = {
		var numPos = 0
		var numNeg = 0

		val tokens = SimpleTokenizer(text).map(_.toLowerCase)
		for(token <- tokens)
		{
			if(posWords.contains(token)) numPos += 1
			else if(negWords.contains(token)) numNeg += 1 
		}
		if(numPos > numNeg) "positive"
		else if(numPos < numNeg) "negative"
		else "neutral"
  	}

	def getEmoticonPolarity(text: String): String = {
		var numPos = 0
		var numNeg = 0

		for(posEmot <- posEmots)
			if(text.contains(posEmot)) numPos += 1
		for(negEmot <- negEmots)
			if(text.contains(negEmot)) numNeg += 1

		if(numPos > numNeg) "posEmoticon"
		else if(numPos < numNeg) "negEmoticon"
		else "neuEmoticon"
  	}

}
