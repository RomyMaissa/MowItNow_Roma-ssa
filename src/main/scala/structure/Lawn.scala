package structure

class Lawn(private val n: Int, private val m: Int, private val instructions: List[(Tondeuse, String)], val display: Boolean = false)  {

  /**
   * playInstructions boucle sur les instructions
   * afin de jouer les sequences une par unes
   */
  def playInstructions(): Unit = {
    instructions.foreach(instruction => {
      val sequence: String = instruction._2
      val tondeuse: Tondeuse = instruction._1
      sequence.foreach(currentMove => {
        playSequence(tondeuse: Tondeuse, currentMove: Char)
      })
      println("Final coord : " + tondeuse.toString)
      println("\n")
    })
  }

  /**
   * Effectue le deplacement pour une tendeuse
   *
   * @param tondeuse
   * @param move
   */
  def playSequence(tondeuse: Tondeuse, move: Char): Unit = {
    if (display) {
      print(tondeuse.toString() + " + ")
    }
    move match {
      case 'G' => tondeuse.depGauche()
      case 'D' => tondeuse.depDroit()
      case 'A' => moveForward(tondeuse)
      case _ => println(move + " : skipped unknown instruction")
    }
    if (display) {
      println(move + " = " + tondeuse.toString())
      displayLawn()
    }
  }

  /**
   * On se déplace vers l'avant si le deplacement
   * est authorisé
   *
   * @param tondeuse
   */
  def moveForward(tondeuse: Tondeuse): Unit = {
    if (authorize(tondeuse)) {
      tondeuse.avancerDroit()
    }
  }


  /**
   * On verifie qu'on est dans la pelouse
   * et que les tondeuses n'entrent pas en collision
   *
   * @param tondeuse
   * @return Boolean
   */
  def authorize(tondeuse: Tondeuse): Boolean = {
    val coord = tondeuse.coorAvancerDroit()
    val x = coord._1
    val y = coord._2

    if (x < 0 | x > n | y < 0 | y > m) {
      println("Reste à la clairière ")
      return false
    }

    instructions.foreach(instruction => {
      val coordLawn = instruction._1.getCoord()
      val xLawn = coordLawn._1
      val yLawn = coordLawn._2
      if (x == xLawn && y == yLawn) {
        println("move aborted ")
        return false
      }
    })
    true
  }

  /**
   * On affiche une grille N x M avec les tondeuses identifées par leurs indices
   */
  def displayLawn(): Unit = {
    var coords: List[(Int, Int)] = List()
    instructions.foreach(instruction => {
      val coordLawn = instruction._1.getCoord()
      val xLawn = coordLawn._1
      val yLawn = coordLawn._2
      coords = coords :+ ((xLawn, yLawn))
    })
    for (y <- n to 0 by -1) {
      for (x <- 0 to m) {
        if (coords.contains((x, y))) {
          print(" " + coords.indexOf((x, y)) + " ")
        } else {
          print(" . ")
        }
      }
      println()
    }
    println()
  }

}
