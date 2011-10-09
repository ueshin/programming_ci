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
package st.happy_camper.programming_ci.optimization

import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File

import scala.math.pow
import scala.math.sqrt

import javax.imageio.ImageIO

/**
 * @author ueshin
 */
object SocialNetwork {

  /*
   * 5.10.1 レイアウト問題
   */
  sealed abstract class People(val name: String)
  object Charlie extends People("Charlie")
  object Augustus extends People("Augustus")
  object Veruca extends People("Veruca")
  object Violet extends People("Violet")
  object Mike extends People("Mike")
  object Joe extends People("Joe")
  object Willy extends People("Willy")
  object Miranda extends People("Miranda")

  val people = List(Charlie, Augustus, Veruca, Violet, Mike, Joe, Willy, Miranda)

  val links = List(
    (Augustus -> Willy),
    (Mike -> Joe),
    (Miranda -> Mike),
    (Violet -> Augustus),
    (Miranda -> Willy),
    (Charlie -> Mike),
    (Veruca -> Joe),
    (Miranda -> Augustus),
    (Willy -> Augustus),
    (Joe -> Charlie),
    (Veruca -> Augustus),
    (Miranda, Joe))

  /*
   * 5.10.2 交差線のカウント
   */
  val domain = people.flatMap { _ => List((10, 370), (10, 370)) }

  /**
   * @param vec
   * @return
   */
  def crossCount(vec: List[Int]): Double = {
    assert(vec.size == people.size * 2)

    val loc = (0 until people.size).map { i =>
      people(i) -> (vec(2 * i), vec(2 * i + 1))
    }.toMap

    val total = (for (i <- 0 until links.size; j <- i + 1 until links.size) yield {
      val ((x1, y1), (x2, y2)) = (loc(links(i)._1), loc(links(i)._2))
      val ((x3, y3), (x4, y4)) = (loc(links(j)._1), loc(links(j)._2))

      val den = (y4 - y3) * (x2 - x1) - (x4 - x3) * (y2 - y1)
      if (den == 0) {
        0
      } else {
        val ua = ((x4 - x3) * (y1 - y3) - (y4 - y3) * (x1 - x3)) / den.toDouble
        val ub = ((x2 - x1) * (y1 - y3) - (y2 - y1) * (x1 - x3)) / den.toDouble
        if (ua > 0 && ua < 1 && ub > 0 && ub < 1) 1 else 0
      }
    }).sum

    total + (for (i <- 0 until people.size; j <- i + 1 until people.size) yield {
      val ((x1, y1), (x2, y2)) = (loc(people(i)), loc(people(j)))
      val dist = sqrt(pow(x1 - x2, 2) + pow(y1 - y2, 2))
      if (dist < 50) { 1 - dist / 50 } else 0
    }).sum
  }

  /*
   * 5.10.3 ネットワークの描画
   */
  /**
   * @param sol
   */
  def drawNetwork(sol: List[Int], jpeg: String): Unit = {
    val pos = (0 until people.size).map { i =>
      people(i) -> (sol(2 * i), sol(2 * i + 1))
    }.toMap

    val im = new BufferedImage(400, 400, BufferedImage.TYPE_INT_RGB)
    val g = im.createGraphics()
    g.setColor(Color.WHITE)
    g.fill(new Rectangle2D.Double(0, 0, 400, 400))

    g.setColor(Color.RED)
    links.foreach {
      case (a, b) =>
        g.draw(new Line2D.Double(pos(a)._1, pos(a)._2, pos(b)._1, pos(b)._2))
    }

    g.setColor(Color.BLACK)
    pos.foreach {
      case (p, (x, y)) =>
        g.drawString(p.name, x, y)
    }

    g.drawImage(im, null, 0, 0)

    ImageIO.write(im, "jpeg", new File(jpeg))
  }
}
