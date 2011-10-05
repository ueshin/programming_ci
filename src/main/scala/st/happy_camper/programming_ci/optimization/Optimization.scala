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

import scala.io.Source

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
      println(line)
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
}
