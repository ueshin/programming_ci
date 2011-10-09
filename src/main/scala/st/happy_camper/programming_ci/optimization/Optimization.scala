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
import scala.io.Source
import scala.math.max
import scala.math.min
import scala.util.Random

/**
 * @author ueshin
 */
object Optimization {

  /*
   * 5.1 グループ旅行
   */
  val people = List(
    ("Seymour" -> "BOS"),
    ("Franny" -> "DAL"),
    ("Zooey" -> "CAK"),
    ("Walt" -> "MIA"),
    ("Buddy" -> "ORD"),
    ("Les" -> "OMA"))

  val destination = "LGA"

  val flights = using(Source.fromFile("src/test/resources/schedule.txt")) { src =>
    val LineRegex = """([A-Z]{3}),([A-Z]{3}),(\d{1,2}:\d{2}),(\d{1,2}:\d{2}),(\d+)""".r
    src.getLines.foldLeft(Map.empty[(String, String), List[(String, String, Int)]]) { (map, line) =>
      val LineRegex(origin, dest, depart, arrive, price) = line
      map + ((origin, dest) -> (map.getOrElse((origin, dest), List.empty[(String, String, Int)]) :+ (depart, arrive, price.toInt)))
    }
  }

  val MinutesRegexp = """(\d{1,2}):(\d{2})""".r

  /**
   * @param t
   */
  def getMinutes(t: String) = {
    t match {
      case MinutesRegexp(h, m) => h.toInt * 60 + m.toInt
    }
  }

  /*
   * 5.2 解の表現
   */
  /**
   * @param r
   */
  def printSchedule(r: List[Int]): Unit = {
    assert(r.size == people.size * 2)

    for (i <- 0 until r.size / 2) {
      val (name, origin) = people(i)
      val out = flights(origin, destination)(r(2 * i))
      val ret = flights(destination, origin)(r(2 * i + 1))
      println("%10s%10s %5s-%5s $%3s %5s-%5s $%3s".format(
        name, origin,
        out._1, out._2, out._3,
        ret._1, ret._2, ret._3))
    }
  }

  /*
   * 5.3 コスト関数
   */
  /**
   * @param
   * @return
   */
  def scheduleCost(sol: List[Int]): Double = {
    assert(sol.size == people.size * 2)

    val (totalPrice, latestArrival, earliestDep) = (0 until sol.size / 2).foldLeft(0, 0, 24 * 60) {
      case ((totalPrice, latestArrival, earliestDep), d) =>
        val origin = people(d)._2
        val outbound = flights(origin, destination)(sol(2 * d))
        val returnf = flights(destination, origin)(sol(2 * d + 1))

        (totalPrice + outbound._3 + returnf._3,
          max(latestArrival, getMinutes(outbound._2)),
          min(earliestDep, getMinutes(returnf._1)))
    }

    val totalWait = (0 until sol.size / 2).foldLeft(0) { (totalWait, d) =>
      val origin = people(d)._2
      val outbound = flights(origin, destination)(sol(2 * d))
      val returnf = flights(destination, origin)(sol(2 * d + 1))

      totalWait + (latestArrival - getMinutes(outbound._2)) + (getMinutes(returnf._1) - earliestDep)
    }

    totalPrice + totalWait + (if (latestArrival < earliestDep) 50 else 0)
  }

  /*
   * 5.4 ランダムサーチ(無作為探索)
   */
  /**
   * @param domain
   * @param costf
   * @return
   */
  def randomOptimize(domain: List[(Int, Int)], costf: List[Int] => Double) = {
    (1 to 1000).foldLeft(Double.MaxValue, Option.empty[List[Int]]) {
      case ((best, bestr), i) =>
        val r = domain.map { d => Random.nextInt(d._2 - d._1 + 1) + d._1 }
        val cost = costf(r)
        if (cost < best) {
          (cost, Option(r))
        } else {
          (best, bestr)
        }
    }
  }

  /*
   * 5.5 ヒルクライム
   */
  /**
   * @param domain
   * @param costf
   * @return
   */
  def hillclimb(domain: List[(Int, Int)], costf: List[Int] => Double) = {
    @tailrec
    def loop(sol: List[Int]): (Double, Option[List[Int]]) = {
      val neighbors = (0 until sol.size).flatMap { j =>
        List(if (sol(j) > domain(j)._1) { sol.take(j) ::: (sol(j) - 1) :: sol.takeRight(sol.size - j - 1) } else Nil,
          if (sol(j) < domain(j)._2) { sol.take(j) ::: (sol(j) + 1) :: sol.takeRight(sol.size - j - 1) } else Nil)
      }.toList

      val current = costf(sol)
      val (best, bestr) = neighbors.foldLeft(current, sol) {
        case ((best, bestr), Nil) => (best, bestr)
        case ((best, bestr), n) =>
          val cost = costf(n)
          if (cost < best) {
            (cost, n)
          } else {
            (best, bestr)
          }
      }

      if (best == current) {
        (best, Option(bestr))
      } else {
        loop(bestr)
      }
    }
    loop(domain.map { d => Random.nextInt(d._2 - d._1 + 1) + d._1 })
  }
}