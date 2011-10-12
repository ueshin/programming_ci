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
package filtering

import java.sql.DriverManager

import scala.collection.mutable
import scala.math.exp
import scala.math.log
import scala.math.min

/**
 * @author ueshin
 */
object DocClass {

  /*
   * 6.2 ドキュメントと単語
   */
  /**
   * @param doc
   * @return
   */
  def getWords(doc: String) = {
    doc.split("""\W+""").filter(w => w.size > 2 && w.size < 20).map(_.toLowerCase).toSet
  }

  /*
   * 6.3 分類器のトレーニング
   */
  /**
   * @author ueshin
   */
  abstract class Classifier(getFeatures: String => Set[String], dbfile: String) {

    /**
     * @param item
     * @param cat
     */
    def train(item: String, cat: String): Unit = {
      getFeatures(item).foreach { f =>
        incf(f, cat)
      }
      incc(cat)
    }

    /*
     * 6.4 確率を計算する
     */
    /**
     * @param f
     * @param cat
     * @return
     */
    def fprob(f: String, cat: String) = {
      if (catcount(cat) == 0) {
        0
      } else {
        fcount(f, cat) / catcount(cat)
      }
    }

    /*
     * 6.4.1 推測を始める
     */
    /**
     * @param f
     * @param cat
     * @param prf
     * @param weight
     * @param ap
     * @return
     */
    def weightedProb(f: String, cat: String, prf: (String, String) => Double, weight: Double = 1.0, ap: Double = 0.5) = {
      val basicProb = prf(f, cat)
      val totals = categories.map { c => fcount(f, c) }.sum
      ((weight * ap) + (totals * basicProb)) / (weight + totals)
    }

    /*
     * 6.7.1 SQLiteを利用する
     */
    Class.forName("org.sqlite.JDBC")

    val conn = DriverManager.getConnection("jdbc:sqlite:" + dbfile)
    using(conn.createStatement()) { stmt =>
      stmt.executeUpdate("create table if not exists fc(feature, category, count)")
      stmt.executeUpdate("create table if not exists cc(category, count)")
    }

    def close() = conn.close()

    /**
     * @param f
     * @param cat
     */
    def incf(f: String, cat: String): Unit = {
      val count = fcount(f, cat)
      if (count == 0) {
        using(conn.prepareStatement("insert into fc values (?, ?, 1)")) { ps =>
          ps.setString(1, f)
          ps.setString(2, cat)
          ps.executeUpdate()
        }
      } else {
        using(conn.prepareStatement("update fc set count = ? where feature = ? and category = ?")) { ps =>
          ps.setInt(1, count)
          ps.setString(2, f)
          ps.setString(3, cat)
          ps.executeUpdate()
        }
      }
    }

    /**
     * @param f
     * @param cat
     * @return
     */
    def fcount(f: String, cat: String) = {
      using(conn.prepareStatement("select count from fc where feature = ? and category = ?")) { ps =>
        ps.setString(1, f)
        ps.setString(2, cat)
        using(ps.executeQuery()) { rs =>
          if (rs.next) rs.getInt(1) else 0
        }
      }
    }

    /**
     * @param cat
     */
    def incc(cat: String): Unit = {
      val count = catcount(cat)
      if (count == 0) {
        using(conn.prepareStatement("insert into cc values (?, 1)")) { ps =>
          ps.setString(1, cat)
          ps.executeUpdate()
        }
      } else {
        using(conn.prepareStatement("update cc set count = ? where category = ?")) { ps =>
          ps.setInt(1, count)
          ps.setString(2, cat)
          ps.executeUpdate()
        }
      }
    }

    /**
     * @param cat
     * @return
     */
    def catcount(cat: String) = {
      using(conn.prepareStatement("select count from cc where category = ?")) { ps =>
        ps.setString(1, cat)
        using(ps.executeQuery()) { rs =>
          if (rs.next) rs.getInt(1) else 0
        }
      }
    }

    /**
     * @return
     */
    def categories = {
      using(conn.prepareStatement("select category from cc")) { ps =>
        using(ps.executeQuery()) { rs =>
          val cs = mutable.Set.empty[String]
          while (rs.next) {
            cs += rs.getString(1)
          }
          cs
        }
      }
    }

    /**
     * @return
     */
    def totalcount() = {
      using(conn.prepareStatement("select sum(count) from cc")) { ps =>
        using(ps.executeQuery()) { rs =>
          if (rs.next) rs.getInt(1) else 0
        }
      }
    }

    /**
     * @param item
     * @param default
     * @return
     */
    def classify(item: String, default: String = "unknown"): String
  }

  /**
   * @param cl
   */
  def sampleTrain(cl: Classifier): Unit = {
    cl.train("Nobody owns the water.", "good")
    cl.train("the quick rabbit jumps fences", "good")
    cl.train("buy pharmaceuticals now", "bad")
    cl.train("make quick money at the online casino", "bad")
    cl.train("the quick brown fox jumps", "good")
  }

  /*
   * 6.5.1 ドキュメント全体の確率
   */
  /**
   * @author ueshin
   */
  class NaiveBayes(getFeatures: String => Set[String], dbfile: String) extends Classifier(getFeatures, dbfile) {

    /**
     * @param item
     * @param cat
     * @return
     */
    def docprob(item: String, cat: String) = {
      getFeatures(item).map { f => weightedProb(f, cat, fprob) }.product
    }

    /*
     * 6.5.2 ベイズの定理の簡単な紹介
     */
    /**
     * @param item
     * @param cat
     * @return
     */
    def prob(item: String, cat: String) = {
      docprob(item, cat) * catcount(cat) / totalcount()
    }

    /*
     * 6.5.3 カテゴリの選択
     */
    val thresholds = mutable.Map.empty[String, Double]

    /**
     * @param item
     * @param default
     * @return
     */
    def classify(item: String, default: String = "unknown") = {
      val (probs, max, best) = categories.foldLeft(Map.empty[String, Double], 0.0, null.asInstanceOf[String]) {
        case ((probs, max, best), cat) =>
          val p = prob(item, cat)
          (probs + (cat -> p), if (p > max) p else max, if (p > max) cat else best)
      }
      if (probs.filterKeys(_ != best).forall(_._2 * thresholds.getOrElse(best, 1.0) < max)) best else default
    }
  }

  /*
   * 6.6.1 特徴たちのカテゴリの確率
   */
  /**
   * @author ueshin
   */
  class FisherClassifier(getFeatures: String => Set[String], dbfile: String) extends Classifier(getFeatures, dbfile) {

    /**
     * @param f
     * @param cat
     * @return
     */
    def cprob(f: String, cat: String) = {
      val clf = fprob(f, cat)
      if (clf == 0.0) {
        0.0
      } else {
        clf / categories.map { c => fprob(f, c) }.sum
      }
    }

    /*
     * 6.6.2 確率を統合する
     */
    /**
     * @param item
     * @param cat
     * @return
     */
    def fisherProb(item: String, cat: String) = {
      val features = getFeatures(item)
      val p = features.foldLeft(1.0) { (p, f) =>
        p * weightedProb(f, cat, cprob)
      }
      val fscore = -2 * log(p)
      invchi2(fscore, 2 * features.size)
    }

    /**
     * @param chi
     * @param df
     * @return
     */
    def invchi2(chi: Double, df: Double) = {
      val m = chi / 2.0
      var term = exp(-m)
      var sum = term
      (1 until (df / 2).toInt).foreach { i =>
        term *= m / i
        sum += term
      }
      min(sum, 1.0)
    }

    /*
     * 6.6.3 アイテムを分類する
     */
    val minimums = mutable.Map.empty[String, Double]

    /**
     * @param item
     * @param default
     * @return
     */
    def classify(item: String, default: String = "unknown") = {
      categories.foldLeft(default, 0.0) {
        case ((best, max), c) =>
          val p = fisherProb(item, c)
          if (p > minimums.getOrElse(c, 0.0) && p > max) {
            (c, p)
          } else {
            (best, max)
          }
      }._1
    }
  }
}
