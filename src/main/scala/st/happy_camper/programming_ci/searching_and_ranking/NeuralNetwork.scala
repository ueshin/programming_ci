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
package searching_and_ranking

import java.sql.DriverManager
import java.sql.SQLException

/*
 * 4.7 クリックからの学習
 */
/**
 * @author ueshin
 */
object NeuralNetwork {

  /*
   * 4.7.2 データベースのセットアップ
   */
  class SearchNet(dbname: String = "") {

    Class.forName("org.sqlite.JDBC")

    val conn = DriverManager.getConnection("jdbc:sqlite:" + dbname)

    /**
     *
     */
    def close() = conn.close()

    /**
     *
     */
    def makeTables() = {
      using(conn.prepareStatement("create table hiddennode(create_key)"))(_.executeUpdate())
      using(conn.prepareStatement("create table wordhidden(fromid, toid, strength)"))(_.executeUpdate())
      using(conn.prepareStatement("create table hiddenurl(fromid, toid, strength)"))(_.executeUpdate())
    }

    /**
     * @param fromid
     * @param toid
     * @param layer
     * @return
     */
    def getStrength(fromid: Int, toid: Int, layer: Int) = {
      val table = if (layer == 0) "wordhidden" else "hiddenurl"
      using(conn.prepareStatement("select strength from %s where fromid = ? and toid = ?".format(table))) { pst =>
        pst.setInt(1, fromid)
        pst.setInt(2, toid)
        using(pst.executeQuery()) { rs =>
          if (rs.next) Some(rs.getDouble(1)) else None
        }
      }.getOrElse(if (layer == 0) -0.2 else 0.0)
    }

    /**
     * @param fromid
     * @param toid
     * @param layer
     * @param strength
     */
    def setStrength(fromid: Int, toid: Int, layer: Int, strength: Double): Unit = {
      val table = if (layer == 0) "wordhidden" else "hiddenurl"
      using(conn.prepareStatement("select rowid from %s where fromid = ? and toid = ?".format(table))) { ps =>
        ps.setInt(1, fromid)
        ps.setInt(2, toid)
        using(ps.executeQuery()) { rs =>
          if (rs.next) Some(rs.getInt(1)) else None
        }
      } match {
        case Some(rowid) =>
          using(conn.prepareStatement("update %s set strength = ? where rowid = ?".format(table))) { ps =>
            ps.setDouble(1, strength)
            ps.setInt(2, rowid)
            ps.executeUpdate()
          }
        case None =>
          using(conn.prepareStatement("insert into %s (fromid, toid, strength) values(?, ?, ?)".format(table))) { ps =>
            ps.setInt(1, fromid)
            ps.setInt(2, toid)
            ps.setDouble(3, strength)
            ps.executeUpdate()
          }
      }
    }

    /**
     * @param wordids
     * @param urlids
     */
    def generateHiddenNode(wordids: (Int, Int), urlids: List[Int]): Unit = {
      val createKey = if (wordids._1 < wordids._2) { wordids._1 + "_" + wordids._2 } else { wordids._2 + "_" + wordids._1 }
      using(conn.prepareStatement("select rowid from hiddennode where create_key = ?")) { ps =>
        ps.setString(1, createKey)
        using(ps.executeQuery()) { rs =>
          if (!rs.next) {
            val hiddenid = using(conn.prepareStatement("insert into hiddennode (create_key) values(?)")) { ps =>
              ps.setString(1, createKey)
              ps.executeUpdate
              using(ps.getGeneratedKeys()) { rs =>
                if (rs.next) rs.getInt(1) else throw new SQLException
              }
            }
            setStrength(wordids._1, hiddenid, 0, 1.0 / 2.0)
            setStrength(wordids._2, hiddenid, 0, 1.0 / 2.0)
            urlids.foreach { urlid =>
              setStrength(hiddenid, urlid, 1, 0.1)
            }
          }
        }
      }
    }
  }

}
