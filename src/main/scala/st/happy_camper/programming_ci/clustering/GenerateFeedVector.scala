/*
 * Copyright 2011 Happy-Camper Street.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 * either express or implied. See the License for the specific language
 * governing permissions and limitations under the License.
 */
package st.happy_camper.programming_ci
package clustering

import java.io.BufferedWriter
import java.io.FileOutputStream
import java.io.OutputStreamWriter
import java.net.URL

import scala.collection.SortedMap
import scala.collection.mutable
import scala.io.Source
import scala.xml.parsing.ConstructingParser

/**
 * @author ueshin
 */
object GenerateFeedVector extends App {

  /*
   * 3.3.2 フィード中の単語を数える
   */
  /**
   * @param url
   * @return
   */
  def getWordCounts(url: String) = {
    println(url)
    using(Source.fromInputStream(new URL(url).openStream)) { src =>
      val d = ConstructingParser.fromSource(src, false).document
      val wc = mutable.Map.empty[String, Long]
      ((d \\ "entry") ++ (d \\ "item")).foreach { entry =>
        (entry \ "summary").orElse(entry \ "description")(0).text.toLowerCase.split("\\W+").foreach { word =>
          if (word != "") {
            wc += (word -> (wc.getOrElse(word, 0L) + 1L))
          }
        }
      }
      (d \\ "title")(0).text -> wc.toMap
    }
  }

  val feedlisttxt = args(0)
  val blogdatatxt = args(1)

  val (feedlist, apcount, wordcounts) = using(Source.fromFile(feedlisttxt)) { src =>
    val feedlist = src.getLines.toList
    val apcount = mutable.Map.empty[String, Long]
    val wordcounts = mutable.Map.empty[String, Map[String, Long]]
    feedlist.foreach { line =>
      try {
        val (title, wc) = getWordCounts(line)
        wordcounts += (title -> wc)
        wc.foreach {
          case (word, count) =>
            apcount += (word -> (apcount.getOrElse(word, 0L) + (if (count > 0L) 1L else 0L)))
        }
      } catch {
        case e => e.printStackTrace
      }
    }
    (feedlist, apcount.toMap, wordcounts ++: SortedMap.empty[String, Map[String, Long]])
  }

  val wordlist = apcount.flatMap {
    case (word, count) =>
      val frac = (count.toDouble / feedlist.size)
      if (frac > 0.1 && frac < 0.5) { Some(word) } else None
  }.toList.sorted

  using(new BufferedWriter(new OutputStreamWriter(new FileOutputStream(blogdatatxt)))) { writer =>
    writer.append("Blog")
    wordlist.foreach { word =>
      writer.append("\t" + word)
    }
    writer.newLine
    wordcounts.foreach {
      case (title, counts) =>
        writer.append(title)
        wordlist.foreach { word =>
          writer.append("\t" + counts.getOrElse(word, 0))
        }
        writer.newLine
    }
  }

}
