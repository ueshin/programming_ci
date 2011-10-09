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
import scala.math.abs
import scala.math.exp
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

  /*
   * 5.6 模擬アニーリング
   */
  /**
   * @param domain
   * @param costf
   * @param t
   * @param cool
   * @param step
   * @return
   */
  def annealingOptimize(domain: List[(Int, Int)], costf: List[Int] => Double, t: Double = 10000.0, cool: Double = 0.95, step: Int = 1) = {
    @tailrec
    def loop(vec: List[Int], t: Double): (Double, Option[List[Int]]) = {
      if (t > 0.1) {
        val idx = Random.nextInt(domain.size)
        val dir = Random.nextInt(2 * step + 1) - step
        val vecb = vec.zipWithIndex.map {
          case (v, i) =>
            if (i == idx) {
              if (v + dir < domain(i)._1) {
                domain(i)._1
              } else if (v + dir > domain(i)._2) {
                domain(i)._2
              } else {
                v + dir
              }
            } else {
              v
            }
        }
        val ea = costf(vec)
        val eb = costf(vecb)
        val p = exp(-abs(eb - ea) / t)
        if (eb < ea || Random.nextDouble() < p) {
          loop(vecb, t * cool)
        } else {
          loop(vec, t * cool)
        }
      } else {
        (costf(vec), Option(vec))
      }
    }
    loop(domain.map { d => Random.nextInt(d._2 - d._1 + 1) + d._1 }, t)
  }

  /*
   * 5.7 遺伝アルゴリズム
   */
  /**
   * @param domain
   * @param costf
   * @param popSize
   * @param step
   * @param mutProb
   * @param elite
   * @param maxIter
   * @return
   */
  def geneticOptimize(domain: List[(Int, Int)], costf: List[Int] => Double, popSize: Int = 50, step: Int = 1, mutProb: Double = 0.2, elite: Double = 0.2, maxIter: Int = 100) = {
    val topelite = (elite * popSize).toInt

    @tailrec
    def mutate(vec: List[Int]): List[Int] = {
      val i = Random.nextInt(domain.size)
      if (Random.nextDouble() < 0.5 && vec(i) > domain(i)._1) {
        vec.take(i) ::: (vec(i) - step) :: vec.takeRight(domain.size - i - 1)
      } else if (vec(i) < domain(i)._2) {
        vec.take(i) ::: (vec(i) + step) :: vec.takeRight(domain.size - i - 1)
      } else {
        mutate(vec)
      }
    }

    def crossover(r1: List[Int], r2: List[Int]) = {
      val i = Random.nextInt(domain.size)
      r1.take(i) ::: r2.takeRight(domain.size - i)
    }

    @tailrec
    def loop(pop: List[List[Int]], itr: Int): (Double, Option[List[Int]]) = {
      val ranked = pop.map { p => (costf(p), p) }.sortBy(_._1)
      if (itr > 0) {
        val elite = ranked.take(topelite).map(_._2)
        val nextGen = (topelite until popSize).map { i =>
          if (Random.nextDouble() < mutProb) {
            mutate(elite(Random.nextInt(topelite)))
          } else {
            crossover(elite(Random.nextInt(topelite)), elite(Random.nextInt(topelite)))
          }
        }.toList
        loop(elite ::: nextGen, itr - 1)
      } else {
        (ranked(0)._1, Option(ranked(0)._2))
      }
    }

    loop((0 until popSize).map(p => domain.map { d => Random.nextInt(d._2 - d._1 + 1) + d._1 }).toList, maxIter)
  }
}
