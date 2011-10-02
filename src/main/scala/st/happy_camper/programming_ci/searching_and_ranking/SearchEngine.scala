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
package searching_and_ranking

import java.lang.Class
import java.net.URL
import java.sql.DriverManager

import scala.annotation.tailrec
import scala.xml.parsing.NoBindingFactoryAdapter
import scala.xml.Node

import org.cyberneko.html.parsers.SAXParser
import org.xml.sax.InputSource

/**
 * @author ueshin
 */
object SearchEngine {

  /*
   * 4.1 検索ランキングとは？
   */
  class Crawler(dbname: String = "") {

    Class.forName("org.sqlite.JDBC")

    /*
     * 4.2.2 クローラのコード
     */
    val IgnoreWords = Set("the", "of", "to", "and", "a", "in", "is", "it")

    /**
     * @param pages
     */
    @tailrec
    final def crawl(pages: Set[URL], depth: Int = 2): Unit = {
      if (depth > 0) {
        crawl(pages.flatMap { page =>
          try {
            val xhtml = using(page.openStream()) { in =>
              val parser = new SAXParser
              val adapter = new NoBindingFactoryAdapter
              parser.setContentHandler(adapter)
              parser.parse(new InputSource(in))
              adapter.rootElem
            }
            addToIndex(page, xhtml)

            (xhtml \\ "A").flatMap { link =>
              val url = new URL(page, (link \ "@href").text)
              val linkText = link.text
              addLinkRef(page, url, linkText)
              if (url.getProtocol().startsWith("http") && !isIndexed(url)) Option(url) else None
            }
          } catch {
            case e => println("Could not open " + page); None
          }
        }, depth - 1)
      }
    }

    /**
     * @param url
     * @param xhtml
     */
    def addToIndex(url: URL, xhtml: Node) = {
      println("Indexing " + url)
    }

    /**
     * @param url
     * @return
     */
    def isIndexed(url: URL) = {
      false
    }

    /**
     * @param urlFrom
     * @param urlTo
     * @param linkText
     */
    def addLinkRef(urlFrom: URL, urlTo: URL, linkText: String) = {

    }

    /*
     * 4.3 インデックスの作成
     */
    private val conn = DriverManager.getConnection("jdbc:sqlite:" + dbname)

    /**
     *
     */
    def close() = conn.close()

    /*
     * 4.3.1 スキーマの設定
     */
    /**
     *
     */
    def createIndexTables(): Unit = {
      using(conn.createStatement()) { stmt =>
        stmt.executeUpdate("create table urllist(url)")
        stmt.executeUpdate("create table wordlist(word)")
        stmt.executeUpdate("create table wordlocation(urlid, wordid, location)")
        stmt.executeUpdate("create table link(fromid integer, toid integer)")
        stmt.executeUpdate("create table linkwords(wordid, linkid)")
        stmt.executeUpdate("create index wordidx on wordlist(word)")
        stmt.executeUpdate("create index urlidx on urllist(url)")
        stmt.executeUpdate("create index wordurlidx on wordlocation(wordid)")
        stmt.executeUpdate("create index urltoidx on link(toid)")
        stmt.executeUpdate("create index urlfromidx on link(fromid)")
      }
    }
  }

}
