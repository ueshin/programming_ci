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

import scala.io.Source
import scala.xml.parsing.ConstructingParser
import java.security.MessageDigest

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

}
