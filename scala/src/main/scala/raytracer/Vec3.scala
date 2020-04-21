package raytracer

final case class Vec3(x: Double, y: Double, z: Double) {
  @inline def +(that: Vec3): Vec3 = Vec3(x + that.x, y + that.y, z + that.z)
  @inline def -(that: Vec3): Vec3 = Vec3(x - that.x, y - that.y, z - that.z)
  @inline def *(that: Vec3): Vec3 = Vec3(x * that.x, y * that.y, z * that.z)

  @inline def scale(a: Double): Vec3 = Vec3(a * x, a * y, a * z)
  @inline def dot(that: Vec3): Double = x * that.x + y * that.y + z * that.z
  @inline def normalise: Vec3 = scale (1.0f / math.sqrt(this dot this))
  @inline def cross(that: Vec3): Vec3 = Vec3(y * that.z - z * that.y, z * that.x - x * that.z, x * that.y - y * that.x)
}

object Vec3 {
  final val One = Vec3(1, 1, 1)
}
