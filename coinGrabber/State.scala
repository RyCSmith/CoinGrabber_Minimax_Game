package coinGrabber

case class State(val board: Array[Array[Int]], val pLoc: Array[Int], val cLoc: Array[Int], var p$: Int, var c$:Int) {
  override def toString = "pLoc: " + pLoc(0) + ", " + pLoc(1) + "  cLoc: " + cLoc(0) + ", " + cLoc(1) + 
  "  p$: " + p$ + "  c$: " + c$ + "\n"
}