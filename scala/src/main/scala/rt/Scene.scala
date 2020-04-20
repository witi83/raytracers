package rt

import rt.Bvh.Objs
import rt.Vec3.Pos

final case class Scene(camLookFrom: Pos, camLookAt: Pos, camFov: Double, spheres: List[Sphere]) {
  def toObjsCam(width: Int, height: Int): (Objs, Camera) =
    (Bvh(_.aabb, spheres), Camera(camLookFrom, camLookAt, Vec3(0, 1, 0), camFov, width.toDouble / height.toDouble))
}

object Scene {

  def fromString(name: String): Option[Scene] = name match {
    case "irreg" => Some(irreg)
    case "rgbbox" => Some(rgbbox)
    case _ => None
  }

  def rgbbox: Scene = {
    val n = 10
    val k = 60.0f
    val leftwall = for {
      y <- 0 until n
      z <- 0 until n
    } yield Sphere(Vec3(-k / 2.0f, -k / 2.0f + k / n * y, -k / 2.0f + k / n * z), Vec3(1, 0, 0), k / (n * 2.0f))

    val midwall = for {
      x <- 0 until n
      y <- 0 until n
    } yield Sphere(Vec3(-k / 2.0f + k / n * x, -k / 2.0f + k / n * y, -k / 2.0f), Vec3(0, 1, 0), k / (n * 2.0f))

    val rightwall = for {
      y <- 0 until n
      z <- 0 until n
    } yield Sphere(Vec3(k / 2.0f, -k / 2.0f + k / n * y, -k / 2.0f + k / n * z), Vec3(0, 0, 1), k / (n * 2.0f))

    val bottom = for {
      x <- 0 until n
      z <- 0 until n
    } yield Sphere(Vec3(-k / 2.0f + k / n * x, -k / 2.0f, -k / 2.0f + k / n * z), Vec3(1, 1, 1), k / (n * 2.0f))

    Scene(Vec3(0, 30, 30), Vec3(0, -1, -1), 75, (leftwall ++ midwall ++ rightwall ++ bottom).toList)
  }

  def irreg: Scene = {
    val n = 100
    val k = 600.0
    val bottom = for {
      x <- 0 until n
      z <- 0 until n
    } yield Sphere(Vec3(-k / 2.0f + k / n * x, 0, -k / 2.0f + k / n * z), Vec3(1, 1, 1), k / (n * 2.0f))

    Scene(Vec3(0, 12, 30), Vec3(0, 10, -1), 75, bottom.toList)
  }
}

