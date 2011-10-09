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
}
