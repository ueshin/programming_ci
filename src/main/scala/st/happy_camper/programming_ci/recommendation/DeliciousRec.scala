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
package st.happy_camper.programming_ci.recommendation

import java.security.MessageDigest

import scala.collection.mutable
import scala.io.Source
import scala.xml.parsing.ConstructingParser

import st.happy_camper.programming_ci.recommendation.Recommendations.Ratings

/**
 * @author ueshin
 */
object DeliciousRec {

  /*
   * 2.6.1 del.icio.usのAPI
   */
  /**
   * @param tag
   */
  def getPopular(tag: String = "programming") = {
    val src = Source.fromURL("http://feeds.delicious.com/v2/rss/popular/" + tag)
    try {
      ConstructingParser.fromSource(src, false).document
    } finally {
      src.close
    }
  }

  /**
   * @param link
   */
  def getUrlPosts(link: String) = {
    val md5 = MessageDigest.getInstance("MD5")
    md5.reset
    md5.update(link.getBytes)
    val md5str = md5.digest.map(0xff & _).foldLeft("")(_ + "%02x".format(_))
    val src = Source.fromURL("http://feeds.delicious.com/v2/rss/url/" + md5str)
    try {
      ConstructingParser.fromSource(src, false).document
    } finally {
      src.close
    }
  }

  /**
   * @param user
   */
  def getUserPosts(user: String) = {
    val src = Source.fromURL("http://feeds.delicious.com/v2/rss/" + user)
    try {
      ConstructingParser.fromSource(src, false).document
    } finally {
      src.close
    }
  }

  /*
   * 2.6.2 データセットを作る
   */
  /**
   * @param tag
   * @param count
   * @return
   */
  def initializeUserDict(tag: String, count: Int = 5): Ratings[String, String] = {
    (getPopular(tag) \\ "item" \ "link").take(count).foldLeft(Map.empty[String, Map[String, Double]]) { (userDict, link) =>
      (getUrlPosts(link.text) \\ "item" \ "creator").foldLeft(userDict) { (userDict, user) =>
        userDict + (user.text -> Map.empty[String, Double])
      }
    }
  }

  /**
   * @param userDict
   */
  def fillItems(userDict: Ratings[String, String]): Ratings[String, String] = {
    val allItems = mutable.Set.empty[String]
    val dict = userDict.keys.foldLeft(userDict) { (userDict, user) =>
      (getUserPosts(user) \\ "item" \ "link").foldLeft(userDict) { (userDict, link) =>
        allItems.add(link.text)
        userDict + (user -> (userDict(user) + (link.text -> 1.0)))
      }
    }
    dict.keys.foldLeft(dict) { (dict, user) =>
      allItems.foldLeft(dict) { (dict, item) =>
        dict + (user -> (dict(user) + (item -> dict(user).getOrElse(item, 0.0))))
      }
    }
  }

}
