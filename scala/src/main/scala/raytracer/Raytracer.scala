package raytracer

object Raytracer {

  type Pos = Vec3
  type Dir = Vec3

  type Color = Vec3
  final val Black: Color = Vec3(0, 0, 0)
  final val White: Color = Vec3(1, 1, 1)

  private[this] final class Go(var min: Double, var max: Double)

  final case class Ray(origin: Pos, dir: Dir) {

    @inline def pointAtParam(t: Double): Pos = origin + dir.scale(t)

    def aabbHit(aabb: AABB, tMin0: Double, tMax0: Double): Boolean = {
      def go(min_ : Double, max_ : Double, origin_ : Double, dir_ : Double, minMax: Go): Unit = {
        val invD = 1.0 / dir_
        val t0 = (min_ - origin_) * invD
        val t1 = (max_ - origin_) * invD
        val t0_ = if (invD < 0) t1 else t0
        val t1_ = if (invD < 0) t0 else t1
        val tMin__ = t0_ max minMax.min
        val tMax__ = t1_ min minMax.max
        minMax.min = tMin__
        minMax.max = tMax__
      }
      val minMax = new Go(tMin0, tMax0)
      go(aabb.min.x, aabb.max.x, origin.x, dir.x, minMax)
      if(minMax.max <= minMax.min) false
      else {
        go(aabb.min.y, aabb.max.y, origin.y, dir.y, minMax)
        if(minMax.max <= minMax.min) false
        else {
          go(aabb.min.z, aabb.max.z, origin.z, dir.z, minMax)
          minMax.max > minMax.min
        }
      }
    }

  }
  final case class Hit(t: Double, p: Pos, normal: Dir, color: Color)
  final case class Sphere(pos: Pos, color: Color, radius: Double) {
    def aabb: AABB = {
      val rVec = Vec3(radius, radius, radius)
      AABB(pos - rVec, pos + rVec)
    }

    def hit(ray: Ray, tMin: Double, tMax: Double): Option[Hit] = {
      val oc = ray.origin - pos
      val a = ray.dir dot ray.dir
      val b = oc dot ray.dir
      val c = (oc dot oc) - radius * radius
      val discriminant = b*b - a*c
      def tryHit(temp: Double) =
        if(temp < tMax && temp > tMin) {
          val point = ray.pointAtParam(temp)
          Some(Hit(temp, point, (point - pos).scale(1.0 / radius), color))
        } else None

      if(discriminant <= 0) None
      else {
        val sqrtDiscriminant = math.sqrt(discriminant)

        tryHit((-b - sqrtDiscriminant) / a) match {
          case s: Some[Hit] => s
          case None => tryHit((-b + sqrtDiscriminant)/a)
        }
      }
    }

  }


  type Objs = BVH[Sphere]

  def objsHit(objs: Objs, ray: Ray, tMin: Double, tMax: Double): Option[Hit] = objs match {
    case Leaf(_, sphere) => sphere.hit(ray, tMin, tMax)
    case Split(box, left, right) =>
      if(!ray.aabbHit(box, tMin, tMax)) None
      else objsHit(left, ray, tMin, tMax) match {
        case opt@Some(hit1) => objsHit(right, ray, tMin, hit1.t).orElse(opt)
        case None => objsHit(right, ray, tMin, tMax)
      }
  }

  final case class Camera(origin: Pos, llc: Pos, horizontal: Dir, vertical: Dir)

  object Camera {
    def apply(lookFrom: Pos, lookAt: Pos, vUp: Dir, vFov: Double, aspect: Double): Camera = {
      val theta = vFov * math.Pi / 180.0
      val halfHeight = math.tan(theta / 2.0)
      val halfWidth = aspect * halfHeight
      val origin = lookFrom
      val w = (lookFrom - lookAt).normalise
      val u = (vUp cross w).normalise
      val v = w cross u
      Camera( lookFrom
            , origin - u.scale(halfWidth) - v.scale(halfHeight) - w
            , u.scale(2*halfWidth)
            , v.scale(2*halfHeight)
            )
    }
  }

  def getRay(cam: Camera, s: Double, t: Double): Ray =
    Ray( cam.origin
       , cam.llc + cam.horizontal.scale(s) + cam.vertical.scale(t) - cam.origin
       )

  @inline def reflect(v: Vec3, n: Vec3): Vec3 =
    v - n.scale(2 * (v dot n))

  @inline def scatter(ray: Ray, hit: Hit): Option[Ray] = {
    val reflected = reflect(ray.dir.normalise, hit.normal)
    if((reflected dot hit.normal) > 0) Some(Ray(hit.p, reflected)) else None
  }

  def rayColor(objs: Objs, ray: Ray, depth: Int): Color = objsHit(objs, ray, 0.001, 1.0 / 0.0) match {
      case Some(hit) => scatter(ray, hit) match {
        case Some(scattered) if depth < 50 =>
          hit.color * rayColor(objs, scattered, depth+1)
        case _ => Black
      }
      case None =>
        val t = 0.5 * (ray.dir.normalise.y + 1.0)
        Vec3.One.scale(1.0-t) + Vec3(0.5, 0.7, 1).scale(t)
  }

  def traceRay(objs: Objs, width: Int, height: Int, camera: Camera, j: Int, i: Int): Color =
    rayColor(objs,
      getRay(camera,
        i.toDouble / width.toDouble,
        j.toDouble / height.toDouble), 0)

  def colorToPixel(c: Color): Image.Pixel =
    ((255.99 * c.x).toByte, (255.99 * c.y).toByte, (255.99 * c.z).toByte)


  def render(objs: Objs, width: Int, height: Int, camera: Camera): Image =
    Image(width, height, (j, i) =>
        colorToPixel(traceRay(objs, width, height, camera, j, i)))

  def time[R](text: String, reps: Int, block: => R): R = {
    val timingHack: Seq[(Double, R)] = (1 to reps).map { _ =>
      val t0 = System.nanoTime()
      val result = block    // call-by-name
      val t1 = System.nanoTime()
      ((t1 - t0)/1000000.0, result)
    }
    val timings = timingHack.map(_._1)
    println(s"$text: ${timings.sum / timings.length}ms (average over $reps repetitions)")
    // First result
    timingHack.head._2
  }
}
