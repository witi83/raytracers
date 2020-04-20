package rt

import java.io.{File, PrintWriter}

import scala.util.Try

object Main {
  def main(args: Array[String]): Unit = {
    val (scene, width, height) = (args match {
      case Array(s, w, h, _*) => for {
        ss <- Scene.fromString(s)
        ww <- Try(w.toInt).toOption
        hh <- Try(h.toInt).toOption
      } yield (ss, ww, hh)
      case _ => None
    }).getOrElse {
      println("Error parsing command line arguments, running rgbbox 1000x1000 px")
      (Scene.rgbbox, 1000, 1000)
    }

    val (objs, cam) = scene.toObjsCam(width, height)
    val out = Image.render(objs, width, height, cam)
    val pw = new PrintWriter(new File("out.ppm"))
    pw.write(out.toPPM)
    println("wrote output to 'out.ppm'")
    pw.close()
  }
}
