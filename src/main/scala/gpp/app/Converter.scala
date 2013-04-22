package gpp.app

import scala.io.Source

trait Converter
{
	def quote(text: String): String = 
	{
		if (text.startsWith("\""))
			if(text.endsWith("\"")) text
			else text + "\""
		else if (text.endsWith("\"")) "\"" + text
		else "\"" + text + "\""
	}
}

object StanfordConverter extends Converter
{
	def main(args: Array[String])
	{
		if (args.isEmpty) {
  			println("Please supply an input filename argument.")
  			System.exit(0)
		}

		val lines = Source.fromFile(args(0)).getLines.toSeq
		val items = lines.map { line =>
			val Array(polarity, tweetid, date, target, username, content) = line.split(";;")
			val label = polarity match
			{
				case "0" => "negative"
				case "2" => "neutral"
				case "4" => "positive"
			}
			val item = "  <item label=" + quote(label) + 
					" tweetid=" + quote(tweetid) +
					" target=" + quote(target) +
					" username=" + quote(username) +
					">\n" +
					"    <content>" + content + "</content>\n" +
					"  </item>\n"
			item
		}
		println("<?xml version=\"1.0\"?>")
		println("<dataset>")
		items.foreach(print)
		println("</dataset>")
	}

	

}

object EmoticonConverter extends Converter
{
	import java.io.File

	def main(args: Array[String])
	{
		if (args.isEmpty) {
  			println("Please supply an input filename argument.")
  			System.exit(0)
		}

		val file = new File(args(0)) 

		println("<?xml version=\"1.0\"?>")
		println("<dataset>")
		extractFeatures(file)
		println("</dataset>")
	}

	def extractFeatures(file: File) 
	{
		if(file.isDirectory)
			file.listFiles.foreach(extractFeatures)
		else if(file.getPath.endsWith(".txt"))
		{
			val lines = Source.fromFile(file.getPath).getLines.toSeq

			val label = if(file.getPath.endsWith("happy.txt")) "positive"
				else if(file.getPath.endsWith("sad.txt")) "negative"
				else "neutral"

			val regex = """([^\t]+)\t([^\t]+)\t(.*)""".r

			val items = lines.map { line =>

				val regex(tweetid, userid, content) = line
			
				val item = "  <item label=" + quote(label) + 
						" tweetid=" + quote(tweetid) +
						" target=\"unknown\"" + 
						" userid=" + quote(userid) +
						">\n" +
						"    <content>" + content + "</content>\n" +
						"  </item>\n"
				item
			}

			items.foreach(print)
		}
	}

}


