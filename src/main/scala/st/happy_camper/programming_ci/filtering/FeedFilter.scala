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
package st.happy_camper.programming_ci.filtering

import scala.io.Source
import scala.xml.parsing.ConstructingParser

import DocClass.Classifier

/*
 * 6.8 Blogフィードをフィルタする
 */
/**
 * @author ueshin
 */
object FeedFilter {

  /**
   * @param feed
   * @param classifier
   */
  def read(feed: String, classifier: Classifier) = {
    val src = Source.fromFile(feed)
    try {
      val feed = ConstructingParser.fromSource(src, false).document
      feed \\ "item" foreach { entry =>
        println
        println("-----")
        println("Title: " + (entry \ "title").text)
        println("Publisher: " + (entry \ "publisher").text)
        println
        println((entry \ "description").text)

        val fulltext = "%s\n%s\n%s".format((entry \ "title").text, (entry \ "publisher").text, (entry \ "description").text)

        println("Guess: " + classifier.classify(fulltext))
        val cat = readLine("Enter category: ")

        classifier.train(fulltext, cat)
      }
    } finally {
      src.close
    }
  }
}
