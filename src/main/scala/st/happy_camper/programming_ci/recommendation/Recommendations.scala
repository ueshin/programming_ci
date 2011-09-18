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

import scala.math.pow
import scala.math.sqrt

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

}
