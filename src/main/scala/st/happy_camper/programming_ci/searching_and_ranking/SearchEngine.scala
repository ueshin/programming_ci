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
import java.sql.SQLException

import scala.annotation.tailrec
import scala.collection.mutable
import scala.math.abs
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
            case e: SQLException => e.printStackTrace; None
            case e               => println("Could not open " + page); None
          }
        }, depth - 1)
      }
    }

    /*
     * 4.3 インデックスの作成
     */
    val conn = DriverManager.getConnection("jdbc:sqlite:" + dbname)

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

    /*
     * 4.3.2 ページ内の単語を探し出す
     */
    /**
     * @param xhtml
     * @return
     */
    def getTextOnly(xhtml: Node) = {
      xhtml.text
    }

    /**
     * @param text
     * @return
     */
    def separateWords(text: String) = {
      text.split("\\W+").flatMap {
        case ""   => None
        case word => Some(word.toLowerCase)
      }.toList
    }

    /*
     * 4.3.3 インデックスへの追加
     */
    /**
     * @param url
     * @param xhtml
     */
    def addToIndex(url: URL, xhtml: Node): Unit = {
      if (!isIndexed(url)) {
        println("Indexing " + url)

        val text = getTextOnly(xhtml)
        val words = separateWords(text)

        val urlid = getEntryId("urllist", "url", url.toString)

        for (i <- 0 until words.size) {
          val word = words(i)
          if (!IgnoreWords.contains(word)) {
            val wordid = getEntryId("wordlist", "word", word)
            using(conn.createStatement()) { stmt =>
              stmt.executeUpdate("insert into wordlocation(urlid, wordid, location) values (%d, %d, %d)".format(urlid, wordid, i))
            }
          }
        }
      }
    }

    /**
     * @param urlFrom
     * @param urlTo
     * @param linkText
     */
    def addLinkRef(urlFrom: URL, urlTo: URL, linkText: String): Unit = {
      val words = separateWords(linkText)
      val fromid = getEntryId("urllist", "url", urlFrom.toString)
      val toid = getEntryId("urllist", "url", urlTo.toString())
      if (fromid != toid) {
        using(conn.createStatement()) { stmt =>
          stmt.executeUpdate("insert into link(fromid, toid) values(%d, %d)".format(fromid, toid))
          val linkid = using(stmt.getGeneratedKeys()) { rs =>
            if (rs.next()) {
              rs.getLong(1)
            } else {
              throw new SQLException
            }
          }
          words.foreach { word =>
            if (!IgnoreWords.contains(word)) {
              val wordid = getEntryId("wordlist", "word", word)
              stmt.executeUpdate("insert into linkwords(linkid, wordid) values(%d, %d)".format(linkid, wordid))
            }
          }
        }
      }
    }

    /**
     * @param table
     * @param field
     * @param value
     * @param createNew
     * @return
     */
    def getEntryId(table: String, field: String, value: String, createNew: Boolean = true): Int = {
      using(conn.createStatement()) { stmt =>
        using(stmt.executeQuery("select rowid from %s where %s = '%s'".format(table, field, value))) { rs =>
          if (rs.next) {
            rs.getInt(1)
          } else {
            stmt.executeUpdate("insert into %s (%s) values ('%s')".format(table, field, value))
            using(stmt.getGeneratedKeys()) { rs =>
              if (rs.next) {
                rs.getInt(1)
              } else {
                throw new SQLException
              }
            }
          }
        }
      }
    }

    /**
     * @param url
     * @return
     */
    def isIndexed(url: URL): Boolean = {
      using(conn.createStatement()) { stmt =>
        using(stmt.executeQuery("select rowid from urllist where url = '%s'".format(url.toString))) { rs =>
          if (rs.next) {
            using(stmt.executeQuery("select * from wordlocation where urlid = %d".format(rs.getInt(1)))) { rs =>
              rs.next
            }
          } else {
            false
          }
        }
      }
    }
  }

  /*
   * 4.4 問い合わせ
   */
  class Searcher(dbname: String) {

    Class.forName("org.sqlite.JDBC")

    val conn = DriverManager.getConnection("jdbc:sqlite:" + dbname)

    /**
     *
     */
    def close() = conn.close()

    /**
     * @param q
     * @return
     */
    def getMatchRows(q: String) = {
      var fieldlist = "w0.urlid"
      var tablelist = ""
      var clauselist = ""
      val wordids = mutable.ListBuffer.empty[Int]

      var tablenumber = 0
      using(conn.createStatement()) { stmt =>
        q.split(" ").foreach { word =>
          val wordrow = using(stmt.executeQuery("select rowid from wordlist where word = '%s'".format(word))) { rs =>
            if (rs.next) Some(rs.getInt(1)) else None
          }
          wordrow.foreach { wordid =>
            wordids += wordid
            if (tablenumber > 0) {
              tablelist += ", "
              clauselist += " and w%d.urlid = w%d.urlid and ".format(tablenumber - 1, tablenumber)
            }
            fieldlist += ", w%d.location".format(tablenumber)
            tablelist += "wordlocation w%d".format(tablenumber)
            clauselist += "w%d.wordid = %d".format(tablenumber, wordid)
            tablenumber += 1
          }
        }

        val fullquery = "select %s from %s where %s".format(fieldlist, tablelist, clauselist)
        val rows = mutable.ListBuffer.empty[List[Int]]
        using(stmt.executeQuery(fullquery)) { rs =>
          while (rs.next) {
            rows += rs.getInt(1) :: (0 until tablenumber).map { i =>
              rs.getInt(i + 2)
            }.toList
          }
        }
        (rows.toList, wordids.toList)
      }
    }

    /*
     * 4.5 内容ベースの順位付け
     */
    /**
     * @param rows
     * @param wordids
     * @return
     */
    def getScoredList(rows: List[List[Int]], wordids: List[Int]) = {
      val totalScores = rows.collect { case id :: row => (id -> 0.0) } ++: mutable.Map.empty[Int, Double]

      val weights = List((1.0, frequencyScore(rows)), (1.5, locationScore(rows)), (2.0, distanceScore(rows)))

      weights.foreach {
        case (weight, scores) =>
          totalScores.foreach {
            case (id, score) =>
              totalScores(id) = score + weight * scores(id)
          }
      }

      totalScores.toList
    }

    /**
     * @param id
     * @return
     */
    def getUrlName(id: Int) = {
      using(conn.createStatement()) { stmt =>
        using(stmt.executeQuery("select url from urllist where rowid = %d".format(id))) { rs =>
          if (rs.next) Some(rs.getString(1)) else None
        }
      }
    }

    /**
     * @param q
     */
    def query(q: String, n: Int = 10): Unit = {
      val (rows, wordids) = getMatchRows(q)
      val scores = getScoredList(rows, wordids)
      val rankedScores = scores.sortBy(-_._2)
      rankedScores.take(n).foreach {
        case (urlid, score) =>
          println(score + "\t" + getUrlName(urlid).getOrElse("-"))
      }
    }

    /*
     * 4.5.1 正規化関数
     */
    /**
     * @param scores
     * @param smallIsBetter
     * @return
     */
    def normalizeScores(scores: Map[Int, Double], smallIsBetter: Boolean = false) = {
      if (smallIsBetter) {
        val min = scores.map(_._2).min
        scores.map {
          case (u, l) =>
            (u -> min / l)
        }
      } else {
        val max = scores.map(_._2).max
        scores.map {
          case (u, c) =>
            (u -> c / max)
        }
      }
    }

    /*
     * 4.5.2 単語の頻度
     */
    /**
     * @param rows
     * @return
     */
    def frequencyScore(rows: List[List[Int]]) = {
      val counts = rows.collect { case id :: row => (id -> 0.0) } ++: mutable.Map.empty[Int, Double]
      rows.collect {
        case id :: row =>
          counts(id) += 1
      }
      normalizeScores(counts.toMap)
    }

    /*
     * 4.5.3 ドキュメント中での位置
     */
    /**
     * @param rows
     * @return
     */
    def locationScore(rows: List[List[Int]]) = {
      val locations = rows.collect { case id :: row => (id -> Double.MaxValue) } ++: mutable.Map.empty[Int, Double]
      rows.collect {
        case id :: row =>
          val loc = row.sum
          if (loc < locations(id)) {
            locations(id) = loc
          }
      }
      normalizeScores(locations.toMap, true)
    }

    /*
     * 4.5.4 単語間の距離
     */
    /**
     * @param rows
     * @return
     */
    def distanceScore(rows: List[List[Int]]) = {
      if (rows(0).size <= 2) {
        rows.collect { case id :: row => id -> 1.0 }.toMap
      } else {
        val minDistance = rows.collect {
          case id :: row =>
            id -> Double.MaxValue
        } ++: mutable.Map.empty[Int, Double]
        rows.collect {
          case id :: row =>
            val dist = (for (i <- 1 until row.size) yield {
              abs(row(i) - row(i - 1))
            }).sum
            if (dist < minDistance(id)) {
              minDistance(id) = dist
            }
        }
        normalizeScores(minDistance.toMap, true)
      }
    }
  }

}
