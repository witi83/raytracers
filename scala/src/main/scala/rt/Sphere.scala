package rt

import rt.Aabb.Go
import rt.Bvh.Objs
import rt.Vec3.{Color, Dir, Pos}

final case class Sphere(pos: Pos, color: Color, radius: Double) {
  def aabb: Aabb = Aabb(
    pos - Vec3(radius, radius, radius),
    pos + Vec3(radius, radius, radius))

  def hit(ray: Ray, tMin: Double, tMax: Double): Option[Hit] = {
    val oc = ray.origin - pos
    val a = ray.dir dot ray.dir
    val b = oc dot ray.dir
    val c = (oc dot oc) - radius * radius
    val discriminant = b * b - a * c

    def tryHit(temp: Double) = if (temp < tMax && temp > tMin)
      Some(Hit(temp, ray.pointAtParam(temp), (ray.pointAtParam(temp) - pos).scale(1.0 / radius), color))
    else None

    if (discriminant <= 0) None
    else tryHit((-b - math.sqrt(b * b - a * c)) / a) match {
      case s: Some[Hit] => s
      case None => tryHit((-b + math.sqrt(b * b - a * c)) / a)
    }
  }
}

final case class Hit(t: Double, p: Pos, normal: Dir, color: Color)

final case class Ray(origin: Pos, dir: Dir) {
  @inline def pointAtParam(t: Double): Pos = origin + dir.scale(t)

    def scatter(hit: Hit): Option[(Ray, Color)] = {
    val reflected = dir.normalise.reflect(hit.normal)
    val scattered = Ray(hit.p, reflected)
    if ((scattered.dir dot hit.normal) > 0) Some((scattered, hit.color)) else None
  }

  def color(objs: Objs, depth: Int): Color = objs.hit(this, 0.001f, 1.0 / 0.0) match { // TODO: rust: 1_000_000_000.0
    case Some(hit) => scatter(hit) match {
      case Some((scattered, attenuation)) if depth < 50 => attenuation * scattered.color(objs, depth + 1)
      case _ => Vec3.Black
    }
    case None =>
      val t = 0.5f * (dir.normalise.y + 1.0f)
      Vec3.One.scale(1.0 - t) + Vec3(0.5, 0.7, 1).scale(t)
  }
}

final case class Aabb(min: Vec3, max: Vec3) {
  @inline def enclosing(that: Aabb): Aabb = Aabb(
    Vec3(math.min(min.x, that.min.x), math.min(min.y, that.min.y), math.min(min.z, that.min.z)),
    Vec3(math.max(max.x, that.max.x), math.max(max.y, that.max.y), math.max(max.z, that.max.z))
  )

  @inline def axis(d: Int): Double = d % 3 match {
    case 0 => centre.x
    case 1 => centre.y
    case _ => centre.z
  }

  @inline def centre: Vec3 = Vec3(min.x + max.x - min.x, min.y + max.y - min.y, min.z + max.z - min.z)

  def hit(ray: Ray, tMin0: Double, tMax0: Double): Boolean = {
    def go(min_ : Double, max_ : Double, origin_ : Double, dir_ : Double, tMin_ : Double, tMax_ : Double): Go = {
      val invD = 1.0f / dir_
      val t0 = (min_ - origin_) * invD
      val t1 = (max_ - origin_) * invD
      val (t0_, t1_) = if (invD < 0.0f) (t1, t0) else (t0, t1)
      new Go(t0_ max tMin_, t1_ min tMax_)
    }

    val go1 = go(min.x, max.x, ray.origin.x, ray.dir.x, tMin0, tMax0)
    if (go1.tMax <= go1.tMin) false
    else {
      val go2 = go(min.y, max.y, ray.origin.y, ray.dir.y, go1.tMin, go1.tMax)
      if (go2.tMax <= go2.tMin) false
      else {
        val go3 = go(min.z, max.z, ray.origin.z, ray.dir.z, go2.tMin, go2.tMax)
        go3.tMax > go3.tMin
      }
    }
  }
}

object Aabb {

  private final class Go(val tMin: Double, val tMax: Double)

}

sealed trait Bvh[A] extends Product with Serializable {
  def aabb: Aabb

  def hit(ray: Ray, tMin: Double, tMax: Double): Option[Hit] = this match {
    case Leaf(_, sphere: Sphere) => sphere.hit(ray, tMin, tMax)
    case Split(box, left, right) =>
      if (!box.hit(ray, tMin, tMax)) None
      else left.hit(ray, tMin, tMax) match {
        case Some(hit1) => Some(right.hit(ray, tMin, hit1.t).getOrElse(hit1))
        case None => right.hit(ray, tMin, tMax)
      }
  }
}

object Bvh {
  type Objs = Bvh[Sphere]

  import scala.math.Ordering.Double.IeeeOrdering

  def apply[A](f: A => Aabb, allObjs: List[A]): Bvh[A] = {

    def go(d: Int, n: Int, objs: List[A]): Bvh[A] = objs match {
      case Nil => throw new RuntimeException("BVH.apply: empty no nodes")
      case x :: Nil => Leaf(f(x), x)
      case xs =>
        val (xsLeft, xsRight) = xs.sortBy(a => f(a).axis(d)).splitAt(n / 2) // TODO: parallel
        def doLeft() = go(d + 1, n / 2, xsLeft)

        def doRight() = go(d + 1, n - n / 2, xsRight)

        val (left, right) = if (n < 100) {
          (doLeft(), doRight())
        } else {
          (doLeft(), doRight()) // TODO: parallel?
          //val l = Future { doLeft() }
          //val r = Future { doRight() }
          //(Await.result(l, Duration.Inf), Await.result(r, Duration.Inf))
        }
        Split(left.aabb.enclosing(right.aabb), left, right)
    }

    go(0, allObjs.length, allObjs)
  }
}

final case class Leaf[A](aabb: Aabb, a: A) extends Bvh[A]

final case class Split[A](aabb: Aabb, left: Bvh[A], right: Bvh[A]) extends Bvh[A]

final case class Camera(origin: Pos, llc: Pos, horizontal: Dir, vertical: Dir) {
  def ray(s: Double, t: Double): Ray = Ray(origin, llc + horizontal.scale(s) + vertical.scale(t) - origin)
}

object Camera {
  def apply(lookFrom: Pos, lookAt: Pos, vUp: Dir, vFov: Double, aspect: Double): Camera = {
    val theta = vFov * math.Pi.floatValue / 180.0f
    val halfHeight = math.tan(theta / 2.0f)
    val halfWidth = aspect * halfHeight
    val w = (lookFrom - lookAt).normalise
    val u = (vUp cross w).normalise
    val v = w cross u
    Camera(lookFrom, lookFrom - u.scale(halfWidth) - v.scale(halfHeight) - w, u.scale(2 * halfWidth),
      v.scale(2 * halfHeight)
    )
  }
}