package tomerghelber.mathematica.interpreter

import org.scalatest.{FunSpec, Matchers}
import tomerghelber.mathematica.eval.MathematicaEvaluator
import tomerghelber.mathematica.parser.MathematicaParser

/**
 * @author user
 * @since 01-Nov-19
 */
class MathematicaInterpreterSpec extends FunSpec with Matchers {

  describe("Examples from WolfRam site") {
    ignore("https://www.wolfram.com/language/gallery/implement-hello-world-in-the-cloud/") {
      val interpreter = new MathematicaInterpreter(
        new MathematicaParser(),
        new MathematicaEvaluator()
      )
      val out1 = interpreter.run("\"Hello, World\"")
      val out2 = interpreter.run("Do[Print[\"Hello, World\"], {5}]")
      val out3 = interpreter.run("CloudObject[\"Hello, World\"]")
      val out4 = interpreter.run(
        "CloudDeploy[\n" +
        " ExportForm[Style[Framed[\"Hello, World\", ImageMargins -> 60],\n" +
        "   80, Orange, FontFamily -> \"Verdana\"], \"GIF\"], \n" +
        " Permissions -> \"Public\"]"
      )
    }

    ignore("https://www.wolfram.com/language/gallery/make-a-hipstamatic-filter/") {
      val interpreter = new MathematicaInterpreter(
        new MathematicaParser(),
        new MathematicaEvaluator()
      )
      val out1 = interpreter.run(
        "CloudDeploy[\n" +
          " FormFunction[FormObject[{\"ImageURL\" -> \"String\"}], \n" +
          "  ImageEffect[\n" +
          "    ColorConvert[\n" +
          "     ImageMultiply[\n" +
          "      ColorConvert[\n" +
          "       ImageAdd[ImageAdjust[Import[#ImageURL], .2], \n" +
          "        RGBColor[.25, .25, -.1]], \"HSB\"], Hue[1, .7, 1]], \n" +
          "     \"RGB\"], {\"PoissonNoise\", .5}] &, \"JPEG\"], \n" +
          " Permissions -> \"Public\"]"
      )
    }
  }
}
