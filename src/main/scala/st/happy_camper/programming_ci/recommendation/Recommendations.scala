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

}
