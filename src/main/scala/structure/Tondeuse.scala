package structure

class Tondeuse(private var x: Int, private var y: Int, private var direction: Char) {

  /**
   * On effectue une direction vers la droite
   */
  def depDroit(): Unit = {
    direction = direction match {
      case 'N' => 'E'
      case 'E' => 'S'
      case 'W' => 'N'
      case 'S' => 'W'
    }
  }

  /**
   * On effectue une direction vers la gauche
   */
  def depGauche(): Unit = {
    direction = direction match {
      case 'N' => 'W'
      case 'E' => 'N'
      case 'W' => 'S'
      case 'S' => 'E'
    }
  }

  /**
   * On retourne les future coordonnées d'un déplacement tout droit
   * en fonction de la direction
   * (Sans rien modifier)
   */
  def coorAvancerDroit(): (Int, Int) = {
    direction match {
      case 'N' => (x, y + 1)
      case 'E' => (x + 1, y)
      case 'W' => (x - 1, y)
      case 'S' => (x, y - 1)
    }
  }

  /**
   * On modifie des coordonnées pour un déplacement tout droit
   */
  def avancerDroit(): Unit = {
    direction match {
      case 'N' => y += 1
      case 'E' => x += 1
      case 'W' => x -= 1
      case 'S' => y -= 1
    }
  }

  /**
   * Retourne les coordonnées de la tondeuse
   *
   * @return (Int, Int)
   */
  def getCoord(): (Int, Int) = {
    (x, y)
  }

  /**
   * to String de la tondeuse
   *
   * @return String
   */
  override def toString: String = {
    "(" + x + "," + y + ") " + direction
  }
}
