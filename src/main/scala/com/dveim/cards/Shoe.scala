package com.dveim.cards

import util.Random

class Shoe(n: Int) {
  var cards = Random.shuffle({
    for (x <- 1 to n) yield Cards.deck
  }.flatten)
}
