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

import scala.collection.mutable

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
  class Classifier(getFeatures: String => Set[String]) {
    val fc = mutable.Map.empty[String, mutable.Map[String, Int]]
    val cc = mutable.Map.empty[String, Int]

    /**
     * @param f
     * @param cat
     */
    def incf(f: String, cat: String): Unit = {
      fc += (f -> fc.getOrElse(f, mutable.Map.empty[String, Int]))
      fc(f) += (cat -> (fc(f).getOrElse(cat, 0) + 1))
    }

    /**
     * @param cat
     */
    def incc(cat: String): Unit = {
      cc += (cat -> (cc.getOrElse(cat, 0) + 1))
    }

    /**
     * @param f
     * @param cat
     * @return
     */
    def fcount(f: String, cat: String) = {
      if (fc.contains(f) && fc(f).contains(cat)) fc(f)(cat) else 0.0
    }

    /**
     * @param cat
     * @return
     */
    def catcount(cat: String) = {
      if (cc.contains(cat)) cc(cat) else 0.0
    }

    /**
     * @return
     */
    def totalcount() = {
      cc.values.sum
    }

    /**
     * @return
     */
    def categories = {
      cc.keySet
    }

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
  class NaiveBayes(getFeatures: String => Set[String]) extends Classifier(getFeatures) {

    /**
     * @param item
     * @param cat
     * @return
     */
    def docprob(item: String, cat: String) = {
      getFeatures(item).map { f => weightedProb(f, cat, fprob) }.product
    }
  }
}
