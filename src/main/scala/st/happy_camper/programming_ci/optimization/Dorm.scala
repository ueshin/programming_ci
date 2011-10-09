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
package optimization

import scala.annotation.tailrec

/**
 * @author ueshin
 */
object Dorm {

  /*
   * 5.9.1 学寮の最適化
   */
  sealed abstract class Dorm(val name: String)
  object Zeus extends Dorm("Zeus")
  object Athena extends Dorm("Athena")
  object Hercules extends Dorm("Hercules")
  object Bacchus extends Dorm("Bacchus")
  object Pluto extends Dorm("Pluto")

  val dorms = List(Zeus, Athena, Hercules, Bacchus, Pluto)

  val prefs = List(
    ("Toby" -> (Bacchus, Hercules)),
    ("Steve" -> (Zeus, Pluto)),
    ("Andrea" -> (Athena, Zeus)),
    ("Sarah" -> (Zeus, Pluto)),
    ("Dave" -> (Athena, Bacchus)),
    ("Jeff" -> (Hercules, Pluto)),
    ("Fred" -> (Pluto, Athena)),
    ("Suzie" -> (Bacchus, Hercules)),
    ("Laura" -> (Bacchus, Hercules)),
    ("Neil" -> (Hercules, Athena)))

  val domain = (0 until dorms.size * 2).map { i => 0 -> (dorms.size * 2 - i - 1) }.toList

  /**
   * @param vec
   */
  def printSolution(vec: List[Int]): Unit = {
    @tailrec
    def loop(i: Int, slot: List[Dorm]): Unit = {
      if (i < vec.size) {
        val x = vec(i)
        val dorm = slot(x)
        println("%6s : %s".format(prefs(i)._1, dorm.name))
        loop(i + 1, slot.take(x) ::: slot.takeRight(slot.size - x - 1))
      }
    }
    loop(0, dorms.flatMap(d => List(d, d)))
  }

  /*
   * 5.9.2 コスト関数
   */
  /**
   * @param vec
   * @return
   */
  def dormCost(vec: List[Int]) = {
    @tailrec
    def loop(i: Int, slot: List[Dorm], cost: Double): Double = {
      if (i < vec.size) {
        val x = vec(i)
        val dorm = slot(x)
        val pref = prefs(i)._2
        loop(i + 1, slot.take(x) ::: slot.takeRight(slot.size - x - 1),
          if (dorm == pref._1) {
            cost
          } else if (dorm == pref._2) {
            cost + 1
          } else {
            cost + 3
          })
      } else {
        cost
      }
    }
    loop(0, dorms.flatMap(d => List(d, d)), 0)
  }
}
