package main
import structure.Tondeuse
import structure.Lawn


object main extends App {
  /*
   On Ouvre le fichier
    */
  val fichierSource = io.Source.fromResource("data.txt")
  val lines = fichierSource.getLines()

  /*
  On Initialise des variables
   */
  var instructions: List[(Tondeuse, String)] = List()
  var xMaxPelouse = -1
  var yMaxPelouse = -1

  /*
  (Pour une taille de 5x5 on a en réalité une grille de 6x6 car 0,0 est inclu)
  On récupère  la premier ligne qui contient les coordonées de la grille
   */
  val coordMaxPelouse: String = lines.next()
  /*
  Lecture des lignes une à une afin créer les mowers et les associer à leurs sequence
  dans la liste instructions
   */
  try {
    xMaxPelouse = coordMaxPelouse.split(" ")(0).toInt
    yMaxPelouse = coordMaxPelouse.split(" ")(1).toInt
    while (lines.hasNext) {
      val moverArray = lines.next().split(" ")
      val tondeuse: Tondeuse = new Tondeuse(moverArray(0).toInt, moverArray(1).toInt, moverArray(2).charAt(0))
      val sequenceString = lines.next()
      instructions = instructions :+ (tondeuse, sequenceString)
    }
    fichierSource.close
    println("\nInstructions : " + instructions + "\n")
  } catch {
    case _: Exception => {
      println("Oups, il y a une erreur")
      System.exit(0)
    }
  }

  if (xMaxPelouse > 0 && yMaxPelouse > 0) {
    /*
    Lawn est ici initialisé en un coup avec tout les parametres.
    Une petite initialisation.
     */
    val lawn = new Lawn(xMaxPelouse, yMaxPelouse, instructions, true)
    lawn.playInstructions()
  } else {
    println("Oups, lawn coord not valid")
  }
}
