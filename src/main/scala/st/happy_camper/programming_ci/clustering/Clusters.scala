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
package clustering

import java.awt.geom.Line2D
import java.awt.geom.Rectangle2D
import java.awt.image.BufferedImage
import java.awt.Color
import java.io.File

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.math.abs
import scala.math.max
import scala.math.pow
import scala.math.random
import scala.math.sqrt
import scala.util.Random

import javax.imageio.ImageIO

/**
 * @author ueshin
 */
object Clusters {

  /*
   * 3.3 階層的クラスタリング
   */
  /**
   * @param filename
   * @return
   */
  def readFile(filename: String) = {
    using(Source.fromFile(filename)) { src =>
      val (header :: lines) = src.getLines.toList
      val (_ :: colnames) = header.split("\t").toList
      val (rownames, data) = lines.foldLeft(List.empty[String], List.empty[List[Double]]) {
        case ((rownames, data), line) =>
          val (rowname :: split) = line.split("\t").toList
          (rownames :+ rowname, data :+ split.map(_.toDouble))
      }
      (rownames, colnames, data)
    }
  }

  /**
   * @param v1
   * @param v2
   * @return
   */
  def pearson(v1: List[Double], v2: List[Double]) = {
    val sum1 = v1.sum
    val sum2 = v2.sum

    val sum1Sq = (v1.map(pow(_, 2.0))).sum
    val sum2Sq = (v2.map(pow(_, 2.0))).sum

    val pSum = v1.zip(v2).map(z => z._1 * z._2).sum

    val num = pSum - (sum1 * sum2 / v1.size)
    val den = sqrt((sum1Sq - pow(sum1, 2.0) / v1.size) * (sum2Sq - pow(sum2, 2.0) / v2.size))

    if (den == 0.0) 0.0 else 1.0 - num / den
  }

  type Distance = (List[Double], List[Double]) => Double

  case class BiCluster(vec: List[Double], id: Int, left: Option[BiCluster] = None, right: Option[BiCluster] = None, distance: Double = 0.0)

  /**
   * @param rows
   * @param distance
   * @return
   */
  def hcluster(rows: List[List[Double]], distance: Distance = pearson) = {
    val distances = mutable.Map.empty[(Int, Int), Double]

    @tailrec
    def hc(clust: List[BiCluster], id: Int = -1): BiCluster = {
      clust match {
        case c :: Nil => c
        case clust =>
          val (closest, lowestpair) = (0 until clust.size).foldLeft(distances.getOrElseUpdate((clust(0).id, clust(1).id), distance(clust(0).vec, clust(1).vec)), (0, 1)) {
            case ((closest, lowestpair), i) => (i + 1 until clust.size).foldLeft(closest, lowestpair) {
              case ((closest, lowestpair), j) =>
                val d = distances.getOrElseUpdate((clust(i).id, clust(j).id), distance(clust(i).vec, clust(j).vec))
                if (d < closest) {
                  (d, (i, j))
                } else {
                  (closest, lowestpair)
                }
            }
          }

          val mergevec = clust(lowestpair._1).vec.zip(clust(lowestpair._2).vec).map(z => (z._1 + z._2) / 2.0)
          val newCluster = BiCluster(mergevec, id, left = Option(clust(lowestpair._1)), right = Option(clust(lowestpair._2)), distance = closest)

          hc((clust.view.filterNot(_ == clust(lowestpair._1)).filterNot(_ == clust(lowestpair._2)) :+ newCluster).toList, id - 1)
      }
    }

    hc(rows.zipWithIndex.map(row => BiCluster(row._1, row._2)))
  }

  /**
   * @param clust
   * @param labels
   */
  def printClust(clust: BiCluster, labels: Option[List[String]] = None, n: Int = 0): Unit = {
    print(" " * n)
    if (clust.id < 0) {
      println("-")
      clust.left.foreach(printClust(_, labels, n + 1))
      clust.right.foreach(printClust(_, labels, n + 1))
    } else {
      println(labels match {
        case Some(l) => l(clust.id)
        case None    => clust.id
      })
    }
  }

  /*
   * 3.4 デンドログラムを描く
   */
  /**
   * @param clust
   * @return
   */
  def getHeight(clust: BiCluster): Double = {
    if (clust.id < 0) {
      clust.left.map(getHeight).getOrElse(0.0) + clust.right.map(getHeight).getOrElse(0.0)
    } else {
      1.0
    }
  }

  /**
   * @param clust
   * @return
   */
  def getDepth(clust: BiCluster): Double = {
    if (clust.id < 0) {
      max(clust.left.map(getDepth).getOrElse(0.0), clust.right.map(getDepth).getOrElse(0.0)) + clust.distance
    } else {
      0.0
    }
  }

  /**
   * @param clust
   * @param labels
   * @param jpeg
   */
  def drawDendrogram(clust: BiCluster, labels: Option[List[String]] = None, jpeg: String = "clusters.jpg"): Unit = {
    val h = getHeight(clust) * 20.0
    val w = 1200.0
    val depth = getDepth(clust)

    val scaling = (w - 150.0) / depth

    val im = new BufferedImage(w.toInt + 120, h.toInt + 20, BufferedImage.TYPE_INT_RGB)
    val g = im.createGraphics()
    g.setColor(Color.WHITE)
    g.fill(new Rectangle2D.Double(0, 0, w + 120.0, h + 20.0))
    g.setColor(Color.RED)
    g.draw(new Line2D.Double(10.0, h / 2 + 10.0, 20.0, h / 2 + 10.0))

    def drawNode(clust: BiCluster, x: Double, y: Double): Unit = {
      if (clust.id < 0) {
        val h1 = clust.left.map(getHeight).getOrElse(0.0) * 20.0
        val h2 = clust.right.map(getHeight).getOrElse(0.0) * 20.0
        val top = y - (h1 + h2) / 2.0
        val bottom = y + (h1 + h2) / 2.0

        val ll = clust.distance * scaling

        g.setColor(Color.RED)
        g.draw(new Line2D.Double(x, top + h1 / 2, x, bottom - h2 / 2))
        g.draw(new Line2D.Double(x, top + h1 / 2, x + ll, top + h1 / 2))
        g.draw(new Line2D.Double(x, bottom - h2 / 2, x + ll, bottom - h2 / 2))

        clust.left.foreach(drawNode(_, x + ll, top + h1 / 2))
        clust.right.foreach(drawNode(_, x + ll, bottom - h2 / 2))
      } else {
        g.setColor(Color.BLACK)
        g.drawString(labels match {
          case Some(l) => l(clust.id)
          case None    => clust.id.toString
        }, x.toInt, y.toInt)
      }
    }

    drawNode(clust, 20, h / 2 + 10)

    g.drawImage(im, null, 0, 0)

    ImageIO.write(im, "jpeg", new File(jpeg))
  }

  /*
   * 3.5 列のクラスタリング
   */
  /**
   * @param data
   * @return
   */
  def rotateMatrix(data: List[List[Double]]) = {
    val result = mutable.ListBuffer.empty[mutable.ListBuffer[Double]]
    (0 until data.map(_.size).max).foreach { i =>
      result += mutable.ListBuffer.empty[Double]
      (0 until data.size).foreach { j =>
        result(i) += (if (data(j).isDefinedAt(i)) data(j)(i) else 0.0)
      }
    }
    result.map(_.toList).toList
  }

  /*
   * 3.6 K平均法によるクラスタリング
   */
  /**
   * @param rows
   * @param k
   * @param distance
   * @return
   */
  def kcluster(rows: List[List[Double]], k: Int = 4, distance: Distance = pearson): List[List[Int]] = {
    val ranges = rotateMatrix(rows).map(row => (row.min, row.max))

    @tailrec
    def iterate(clusters: List[List[Double]], lastmatches: List[mutable.ListBuffer[Int]] = Nil, t: Int = 100): List[List[Int]] = {
      if (t > 0) {
        println("Iteration %d".format(100 - t))
        val bestmatches: List[mutable.ListBuffer[Int]] = (for (_ <- 0 until k) yield { mutable.ListBuffer.empty[Int] }).toList
        for (j <- 0 until rows.size) {
          val row = rows(j)
          var bestmatch = 0
          for (i <- 0 until k) {
            val d = distance(clusters(i), row)
            if (d < distance(clusters(bestmatch), row)) {
              bestmatch = i
            }
          }
          bestmatches(bestmatch) += j
        }

        if (bestmatches == lastmatches) {
          bestmatches.map(_.toList.sorted).sortBy(_(0))
        } else {
          iterate((for (i <- 0 until k) yield {
            if (bestmatches(i).size > 0) {
              val avgs = mutable.ListBuffer((0 until ranges.size).map(_ => 0.0): _*)
              bestmatches(i).foreach { rowid =>
                for (m <- 0 until rows(rowid).size) {
                  avgs(m) += rows(rowid)(m)
                }
                for (j <- 0 until avgs.size) {
                  avgs(j) /= bestmatches(i).size
                }
              }
              avgs.toList
            } else {
              clusters(i)
            }
          }).toList, bestmatches, t - 1)
        }
      } else {
        lastmatches.map(_.toList.sorted).sortBy(_(0))
      }
    }

    iterate((for (_ <- 0 until k) yield {
      (0 until ranges.size).map { i =>
        Random.nextDouble * (ranges(i)._2 - ranges(i)._1) + ranges(i)._1
      }.toList
    }).toList)
  }

  /*
   * 3.7.4 距離の基準を定義する
   */
  /**
   * @param v1
   * @param v2
   * @return
   */
  def tanimoto(v1: List[Double], v2: List[Double]) = {
    val c1 = v1.filter(_ != 0.0).size.toDouble
    val c2 = v2.filter(_ != 0.0).size.toDouble
    val shr = ((v1.zipWithIndex.collect {
      case (v, i) if v != 0.0 => i
    }.toSet) & (v2.zipWithIndex.collect {
      case (v, i) if v != 0.0 => i
    }).toSet).size.toDouble

    1.0 - (shr / (c1 + c2 - shr))
  }

  /*
   * 3.8 データを2次元で見る
   */
  /**
   * @param data
   * @param rate
   * @param distance
   * @return
   */
  def scaleDown(data: List[List[Double]], rate: Double = 0.01, distance: Distance = pearson) = {
    val n = data.size

    val realDist = data.map { i =>
      data.map { j =>
        distance(i, j)
      }
    }

    @tailrec
    def iterate(loc: List[(Double, Double)], lasterror: Double = Double.MaxValue, m: Int = 1000): List[(Double, Double)] = {
      if (m > 0) {
        val fakeDist = loc.map { i =>
          loc.map { j =>
            sqrt(pow(i._1 - j._1, 2) + pow(i._2 - j._2, 2))
          }
        }

        val grad = mutable.ListBuffer((0 until n).map(_ => (0.0, 0.0)): _*)
        var totalerror = 0.0
        for (i <- 0 until n) {
          for (j <- 0 until n if j != i) {
            val errorterm = (fakeDist(j)(i) - realDist(j)(i)) / realDist(j)(i)
            grad(i) = (grad(i)._1 + ((loc(i)._1 - loc(j)._1) / fakeDist(j)(i)) * errorterm,
              grad(i)._2 + ((loc(i)._2 - loc(j)._2) / fakeDist(j)(i)) * errorterm)
            totalerror += abs(errorterm)
          }
        }
        println("%4d".format(m) + ":" + totalerror)

        if (lasterror < totalerror) {
          loc
        } else {
          iterate(loc.zip(grad).map {
            case (loc, grad) =>
              (loc._1 - rate * grad._1, loc._2 - rate * grad._2)
          }, totalerror, m - 1)
        }
      } else {
        loc
      }
    }

    iterate((0 until n).map(_ => (random, random)).toList)
  }

  /**
   * @param data
   * @param labels
   * @param jpeg
   */
  def draw2d(data: List[(Double, Double)], labels: List[String], jpeg: String = "mds2d.jpg"): Unit = {
    val im = new BufferedImage(2000, 2000, BufferedImage.TYPE_INT_RGB)
    val g = im.createGraphics()
    g.setColor(Color.WHITE)
    g.fill(new Rectangle2D.Double(0, 0, 2000, 2000))
    g.setColor(Color.BLACK)
    for (i <- 0 until data.size) {
      g.drawString(labels(i), ((data(i)._1 + 0.5) * 1000).toInt, ((data(i)._2 + 0.5) * 1000).toInt)
    }
    g.drawImage(im, null, 0, 0)

    ImageIO.write(im, "jpeg", new File(jpeg))
  }

}
