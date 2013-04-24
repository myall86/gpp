package gpp.app

import scala.io.Source

import org.apache.log4j.Level
import org.apache.log4j.Logger

import nak.app.Classify
import nak.data._
import nak.liblinear.{LiblinearConfig, SolverType}
import nak.NakContext._
import nak.util.ConfusionMatrix

import gpp.classify._


/**
 * A standalone object with a main method for reading an input file and running
 * twitter polarity classification application with different options.
 */
object SentimentAnalysis {
	

  def main(args: Array[String]) {
    // Parse and get the command-line options
    val opts = SentimentAnalysisOpts(args)
    
    // This tells the algorithm how much debugging information to show you
    // while you run the algorithm. 
    val logLevel = if (opts.verbose()) Level.DEBUG else Level.INFO
    Logger.getRootLogger.setLevel(logLevel)
    
    // TODO
	val GPP_DIR = System.getProperty("gpp.dir")
	val evalLines = opts.evalfiles().flatMap { evalfile =>
				Source.fromFile(evalfile).getLines.toSeq
			}
			.toSeq

	val items = evalLines.filter(line => line.contains("<content>"))
			.map(_.replaceAll("""<content>|</content>""", "").trim)

	val goldLabels = evalLines.filter(line => line.contains("label=\""))
				.map(line => 
					if(line.contains("positive")) "positive"
					else if(line.contains("neutral")) "neutral"
					else if(line.contains("negative")) "negative"
					else ""
				)

	// Filter out irrelevant labels
	val (filteredItems, filterGoldLabels) = items.zip(goldLabels)
							.filterNot(_._2.isEmpty)
							.unzip

	val predictedLabels = opts.method() match
	{
		case "majority" => MajorityMethod(opts.trainfiles(), opts.evalfiles())
		case "lexicon" => LexiconMethod(filteredItems)
		case "L2R_LR" => LiblinearMethod(
					opts.trainfiles(), 
					filteredItems, 
					filterGoldLabels, 
					opts.method(), 
					opts.cost(), 
					0.01, 
					opts.extended(), 
					opts.verbose()
				)
		case _ => Seq[String]()
	}
	
	val confusionMatrix = ConfusionMatrix(filterGoldLabels, predictedLabels, filteredItems)
	if(opts.detailed()) println(confusionMatrix.detailedOutput)
	println(confusionMatrix.toString)
  }

	/* Majority method */
	def MajorityMethod(trainfiles: List[String], evalfiles: List[String]): Seq[String] = 
	{
		val trainLines = trainfiles.flatMap { trainfile =>
				Source.fromFile(trainfile).getLines.toSeq
			}
			.toSeq

		val evalLines = evalfiles.flatMap { evalfile =>
				Source.fromFile(evalfile).getLines.toSeq
			}
			.toSeq
		
		val numPos = trainLines.count(line => line.contains("label=\"positive\""))
		val numNeu = trainLines.count(line => line.contains("label=\"neutral\""))
		val numNeg = trainLines.count(line => line.contains("label=\"negative\""))
		val majorLabel = Seq(("positive", numPos), ("neutral", numNeu), ("negative", numNeg))
				.sortBy(-_._2)
				.head
				._1

		val predictedLabels = evalLines.filter(line => line.contains("label=\"positive\"") ||
								line.contains("label=\"neutral\"") ||
								line.contains("label=\"negative\"")
							)
						.map(_ => majorLabel)
		predictedLabels
	}

	/* Lexicon method */
  	def LexiconMethod(items: Seq[String]): Seq[String] = 
	{
		items.map(ExtendedFeatureExtractor.getPolarity)
  	}

	/* Liblinear method */
  	def LiblinearMethod(
		trainfiles: List[String], 
		items: Seq[String], 
		goldLabels: Seq[String], 
		method: String,
		cost: Double,
		eps: Double = 0.01,
		extended: Boolean,
		verbose: Boolean): Seq[String] = 
	{
		val trainLines = trainfiles.flatMap { trainfile =>
				Source.fromFile(trainfile).getLines.toSeq
			}
			.toSeq
		
		val trainItems = trainLines.filter(line => line.contains("<content>"))
					.map(_.replaceAll("""<content>|</content>""", "").trim)

		val trainLabels = trainLines.filter(line => line.contains("label=\""))
					.map(line => 
						if(line.contains("positive")) "positive"
						else if(line.contains("neutral")) "neutral"
						else if(line.contains("negative")) "negative"
						else ""
					)

		// Filter out irrelevant labels
		val (filteredTrainItems, filterTrainLabels) = trainItems.zip(trainLabels)
									.filterNot(_._2.isEmpty)
									.unzip

		// Choose the solver.
    		val solverType = nak.liblinear.Solver(method)

		// Read and index the examples in the training file.
		val indexer = new ExampleIndexer

		val examples = filteredTrainItems.zip(filterTrainLabels)
					.map { case (trainItem, label) =>
						val features = 
							if (!extended) BasicFeatureExtractor(trainItem)
							else ExtendedFeatureExtractor(trainItem)

						Example(label, features)
					}
					.toList
					.map(indexer)

		val (lmap, fmap) = indexer.getMaps

		// Configure and train with liblinear.
    		val config = new LiblinearConfig(solverType, cost, eps, verbose)
    		val classifier = trainClassifier(config, examples, lmap, fmap)
		
		val evalData = items.zip(goldLabels).map { case (item, label) =>
			val features = 
				if (!extended) BasicFeatureExtractor(item)
				else ExtendedFeatureExtractor(item)

			Example(label, features)
		}
		.toList

		// Get the output distributions for the evaluation data.
      		val predictedLabels = for (ex <- evalData) yield {
			val scores = classifier.evalUnindexed(ex.features)
			val best = scores.zipWithIndex.maxBy(_._1)._2
			classifier.labelOfIndex(best)
      		}

		predictedLabels
  	}

}


/**
 * An object that sets of the configuration for command-line options using
 * Scallop and returns the options, ready for use.
 */
object SentimentAnalysisOpts {

  import org.rogach.scallop._
  
  def apply(args: Array[String]) = new ScallopConf(args) {
    banner("""
Classification application.

For usage see below:
	     """)

	val trainfiles = opt[List[String]]("train", short='t', descr="The list of files containing training events.")
	val evalfiles = opt[List[String]]("eval", short='e', descr="The file containing evalualation events.")
	val solverTypes = Set("majority", "lexicon", "L2R_LR")

	val method = opt[String]("method", short='m', default=Some("L2R_LR"), validate = solverTypes, 
				descr = "The type of solver to use. Possible values: " + solverTypes.toSeq.sorted.mkString(",") ) 

	val cost = opt[Double]("cost", short='c', default=Some(1.0), validate = (0<), 
				descr="The cost parameter C. Bigger values means less regularization (more fidelity to the training set).")

	val extended = opt[Boolean]("extended", short = 'x', descr = "Use extended features.")
	val detailed = opt[Boolean]("detailed", short = 'd')
	val help = opt[Boolean]("help", noshort = true, descr="Show this message")
	val verbose = opt[Boolean]("verbose", short = 'v')
	val version = opt[Boolean]("version", noshort = true, descr="Show version of this program")
  }
}
