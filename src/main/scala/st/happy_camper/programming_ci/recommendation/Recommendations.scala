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

import java.io.File

import scala.collection.mutable
import scala.io.Codec
import scala.io.Source
import scala.math.pow
import scala.math.sqrt

import io.Codec.charset2codec

/**
 * @author ueshin
 */
object Recommendations {

  /*
   * 2.2 嗜好の収集
   */
  type Ratings[A, B] = Map[A, Map[B, Double]]

  val Critics: Ratings[String, String] = Map(
    "Lisa Rose" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 3.0, "Superman Returns" -> 3.5, "You, Me and Dupree" -> 2.5,
      "The Night Listener" -> 3.0),
    "Gene Seymour" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 3.5,
      "Just My Luck" -> 1.5, "Superman Returns" -> 5.0, "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 3.5),
    "Michael Phillips" -> Map("Lady in the Water" -> 2.5, "Snakes on a Plane" -> 3.0,
      "Superman Returns" -> 3.5, "The Night Listener" -> 4.0),
    "Claudia Puig" -> Map("Snakes on a Plane" -> 3.5, "Just My Luck" -> 3.0,
      "The Night Listener" -> 4.5, "Superman Returns" -> 4.0,
      "You, Me and Dupree" -> 2.5),
    "Mick LaSalle" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0,
      "Just My Luck" -> 2.0, "Superman Returns" -> 3.0, "The Night Listener" -> 3.0,
      "You, Me and Dupree" -> 2.0),
    "Jack Matthews" -> Map("Lady in the Water" -> 3.0, "Snakes on a Plane" -> 4.0,
      "The Night Listener" -> 3.0, "Superman Returns" -> 5.0, "You, Me and Dupree" -> 3.5),
    "Toby" -> Map("Snakes on a Plane" -> 4.5, "You, Me and Dupree" -> 1.0, "Superman Returns" -> 4.0))

  /*
   * 2.3.1 ユークリッド距離によるスコア
   */
  /**
   * @param ratings
   * @param a1
   * @param a2
   * @return
   */
  def simDistance[A, B](ratings: Ratings[A, B], a1: A, a2: A) = {
    val si = ratings(a1).keys.collect {
      case it if ratings(a2).contains(it) => it
    }.toList
    if (si.size > 0) {
      1.0 / (1.0 + sqrt((si.map { it =>
        pow(ratings(a1)(it) - ratings(a2)(it), 2.0)
      }).sum))
    } else {
      0.0
    }
  }

  /*
   * 2.3.2 ピアソン相関によるスコア
   */
  /**
   * @param ratings
   * @param a1
   * @param a2
   * @return
   */
  def simPearson[A, B](ratings: Ratings[A, B], a1: A, a2: A) = {
    val si = ratings(a1).keys.collect {
      case it if ratings(a2).contains(it) => it
    }.toList
    if (si.size > 0) {
      val n = si.size

      val sum1 = (si.map { it => ratings(a1)(it) }).sum
      val sum2 = (si.map { it => ratings(a2)(it) }).sum

      val sum1Sq = (si.map { it => pow(ratings(a1)(it), 2.0) }).sum
      val sum2Sq = (si.map { it => pow(ratings(a2)(it), 2.0) }).sum

      val pSum = (si.map { it => ratings(a1)(it) * ratings(a2)(it) }).sum

      val num = pSum - (sum1 * sum2 / n)
      val den = sqrt((sum1Sq - pow(sum1, 2.0) / n) * (sum2Sq - pow(sum2, 2.0) / n))

      if (den == 0.0) 0.0 else num / den
    } else {
      0.0
    }
  }

  /*
   * 2.3.4 評者をランキングする
   */
  type Similarity[A, B] = (Ratings[A, B], A, A) => Double

  /**
   * @param ratings
   * @param a
   * @param n
   * @param similarity
   * @return
   */
  def topMatches[A, B](ratings: Ratings[A, B], a: A, n: Int = 5, similarity: Similarity[A, B] = simPearson[A, B] _) = {
    ratings.keys.collect {
      case o if o != a =>
        (o -> similarity(ratings, a, o))
    }.toList.sortBy { case (o, sim) => -sim }.take(n)
  }

  /*
   * 2.4 アイテムを推薦する
   */
  /**
   * @param ratings
   * @param a
   * @param similarity
   * @return
   */
  def getRecommendations[A, B](ratings: Ratings[A, B], a: A, similarity: Similarity[A, B] = simPearson[A, B] _) = {
    val (totals, simSums) = ratings.keys.filter(_ != a).foldLeft(mutable.Map.empty[B, Double], mutable.Map.empty[B, Double]) {
      case ((totals, simSums), o) =>
        val sim = similarity(ratings, a, o)
        if (sim > 0) {
          ratings(o).keys.filter(ratings(a).getOrElse(_, 0.0) == 0.0).foldLeft(totals, simSums) {
            case ((totals, simSums), it) =>
              (totals += (it -> (totals.getOrElse(it, 0.0) + ratings(o)(it) * sim)),
                simSums += (it -> (simSums.getOrElse(it, 0.0) + sim)))
          }
        } else {
          (totals, simSums)
        }
    }
    totals.map {
      case (it, total) =>
        (it, total / simSums(it))
    }.toList.sortBy { case (it, score) => -score }
  }

  /*
   * 2.5 似ている製品
   */
  /**
   * @param ratings
   * @return
   */
  def transform[A, B](ratings: Ratings[A, B]): Ratings[B, A] = {
    ratings.foldLeft(Map.empty[B, Map[A, Double]]) {
      case (transform, (a, ratings)) =>
        ratings.foldLeft(transform) {
          case (transform, (b, rating)) =>
            transform + (b -> (transform.getOrElse(b, Map.empty[A, Double]) + (a -> rating)))
        }
    }
  }

  val Movies = transform(Critics)

  /*
   * 2.7.1 アイテム間の類似度データセットを作る
   */
  /**
   * @param ratings
   * @param n
   * @param similarity
   * @return
   */
  def calculateSimilarItems[A, B](ratings: Ratings[A, B], n: Int = 10, similarity: Similarity[B, A] = simDistance[B, A] _) = {
    val tr = transform(ratings)
    tr.keys.map { it =>
      it -> topMatches(tr, it, n, similarity)
    }.toMap
  }

  /*
   * 2.7.2 推薦を行う
   */
  /**
   * @param ratings
   * @param itsims
   * @param a
   */
  def getRecommendedItems[A, B](ratings: Ratings[A, B], itsims: Map[B, List[(B, Double)]], a: A) = {
    val (scores, totalSim) = ratings(a).foldLeft(Map.empty[B, Double], Map.empty[B, Double]) {
      case ((scores, totalSim), (it, rating)) =>
        itsims(it).foldLeft(scores, totalSim) {
          case ((scores, totalSim), (it, sim)) if !ratings(a).contains(it) =>
            (scores + (it -> (scores.getOrElse(it, 0.0) + sim * rating)),
              totalSim + (it -> (totalSim.getOrElse(it, 0.0) + sim)))
          case ((scores, totalSim), _) => (scores, totalSim)
        }
    }
    scores.map {
      case (it, score) => (it -> score / totalSim(it))
    }.toList.sortBy { case (it, score) => -score }
  }

  /*
   * 2.8 MovieLensのデータセットを使う
   */
  /**
   * @param path
   * @return
   */
  def loadMovieLens(path: String = "src/test/resources"): Ratings[Int, String] = {
    val ItemLineRegexp = """(\d+)\|([^|]+)\|.*""".r
    val itemSource = Source.fromFile(new File(path, "u.item"))(Codec.ISO8859)
    val items = try {
      itemSource.getLines.map {
        case ItemLineRegexp(movieid, title) => (movieid -> title)
      }.toMap
    } finally {
      itemSource.close
    }

    val DataLineRegexp = """(\d+)\t(\d+)\t(\d+)\t(\d+)""".r
    val dataSource = Source.fromFile(new File(path, "u.data"))
    try {
      dataSource.getLines.foldLeft(Map.empty[Int, Map[String, Double]]) {
        case (ratings, DataLineRegexp(user, movieid, rating, ts)) =>
          ratings + (user.toInt -> (ratings.getOrElse(user.toInt, Map.empty[String, Double]) + (items(movieid) -> rating.toDouble)))
        case (ratings, _) => ratings
      }
    } finally {
      dataSource.close
    }
  }

  /*
   * 2.10.1 Tanimoto係数
   */
  /**
   * @param ratings
   * @param a1
   * @param a2
   * @return
   */
  def simTanimotoR[A, B](ratings: Ratings[A, B], a1: A, a2: A) = {
    val si = (ratings(a1).keySet ++ ratings(a2).keySet).toList
    if (si.size > 0) {
      val sum1Sq = (si.map { it => pow(ratings(a1).getOrElse(it, 0.0), 2.0) }).sum
      val sum2Sq = (si.map { it => pow(ratings(a2).getOrElse(it, 0.0), 2.0) }).sum

      val ip = (si.map { it => ratings(a1).getOrElse(it, 0.0) * ratings(a2).getOrElse(it, 0.0) }).sum

      ip / (sum1Sq + sum2Sq - ip)
    } else {
      0.0
    }
  }

  /**
   * @param ratings
   * @param a1
   * @param a2
   * @return
   */
  def simTanimotoB[A, B](ratings: Ratings[A, B], a1: A, a2: A) = {
    val sisize = (ratings(a1).keySet & ratings(a2).keySet).size
    if (sisize > 0) {
      sisize.asInstanceOf[Double] / (ratings(a1).size + ratings(a2).size - sisize)
    } else {
      0.0
    }
  }

}
