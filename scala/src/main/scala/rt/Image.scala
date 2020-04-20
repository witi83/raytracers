package rt

import rt.Bvh.Objs
import rt.Vec3.Color

import scala.collection.parallel.immutable.ParSeq

final case class Image(width: Int, height: Int, pixels: Image.PixelData) {
  // TODO: use writer
  def toPPM: String = {
    val pixel2ppm = (p: Image.Pixel) => s"${p._1 & 0xFF} ${p._2 & 0xFF} ${p._3 & 0xFF}"

    (Seq("P3", s"$width $height", "255") ++ pixels.map(pixel2ppm)).mkString("\n") + "\n"
  }
}

object Image {
  type Pixel = (Byte, Byte, Byte) // TODO: Int?
  type PixelData = ParSeq[Pixel]

  def render(objs: Objs, width: Int, height: Int, camera: Camera): Image =
    Image(width, height, (j, i) => Pixel(traceRay(objs, width, height, camera, j, i)))

  def apply(width: Int, height: Int, pixel: (Int, Int) => Pixel): Image =
    Image(width, height,
      ParSeq.tabulate(width * height)(n => (height - n / height, n % width)) // TODO: Rust: j = height - l / width;
        .map(pixel.tupled))

  def traceRay(objs: Objs, width: Int, height: Int, cam: Camera, j: Int, i: Int): Color =
    cam.ray(i / width, j / height).color(objs, 0)
}

object Pixel {
  def apply(c: Color): Image.Pixel = ((255.99 * c.x).toByte, (255.99 * c.y).toByte, (255.99 * c.z).toByte)
}