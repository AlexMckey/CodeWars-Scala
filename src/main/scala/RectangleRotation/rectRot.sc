val x = 8
val y = 6
def rot(p: (Double,Double), a: Int): (Double,Double) = {
  val s = math.sin(math.toRadians(a))
  val c = math.cos(math.toRadians(a))
  val (x,y) = p
  (x*c-y*s,x*s+y*c)
}
val rs: List[(Double,Double)] = List((x/2,y/2),(x/2,-y/2),(-x/2,y/2),(-x/2,-y/2))
rs.map(p => rot(p,45))
  .map{ case (x,y) => (x.toInt+x.sign*1,y.toInt+y.sign*1)}
  .map{ case (x,y) => math.sqrt(x*x+y*y)}

val (x1,y1) = rot((-x/2,y/2),45)
val (x2,y2) = rot((-x/2,-y/2),45)
val (x3,y3) = rot((x/2,-y/2),45)
val (x4,y4) = rot((x/2,y/2),45)

val d = (x3-x2)*(y1-y2)-(x1-x2)*(y3-y2)
def check(x: Int, y: Int): Boolean = {
  val cx = (y1-y2)*(x-x2)-(x1-x2)*(y-y2)
  val cy = (x3-x2)*(y-y2)-(y3-y2)*(x-x2)
  cx>=0 && cx<=d && cy >= 0 && cy <= d
}

val r = math.max(x/2,y/2)
val ps = Range(-r,r+1).flatMap(x => Range(-r,r+1).map(y => (x,y)))
ps.count{ case (x,y) => check(x,y) }