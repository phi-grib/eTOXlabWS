package controllers

import java.io.File
import java.nio.file.Path

import models.dataframe.DataFrame
import play.api.libs.json._

import scala.sys.process._

object eTOXlab {

  val etoxlabhome = Application.conf.getString("etoxlabhome")
  val modeldir = etoxlabhome + "/src"

  private def getPredictionRAW(modelId: String, molFile: String, tmpDir: Path) = {

    println("Model tag: " + modelId)
    println("Models...")
    models.map(println)
    val (model, tag, iv, ev) = eTOXlab.models(modelId)
    println(model, tag, iv, ev)
    val f = new File(tmpDir.toAbsolutePath().toString())
    val p = Process(eTOXlab.callPredictEtoxLab(model, iv, molFile), cwd = Some(f))

    println(eTOXlab.callPredictEtoxLab(tag, iv, molFile))
    val q = p.!

    scala.io.Source.fromFile(tmpDir.toAbsolutePath().toString() + "/result.txt").getLines
  }

  def getPredictionJSON(modelId: String, molFile: String, tmpDir: Path) = {
    eTOXlab.parseResults(this.getPredictionRAW(modelId, molFile, tmpDir))
  }

  def getPredictionDF(modelId: String, molFile: String, tmpDir: Path) = {
    eTOXlab.parseResults_table(this.getPredictionRAW(modelId, molFile, tmpDir))
  }

  def callPredictEtoxLab(model: String, version: String, fileCmps: String) = {
    val cmd = Seq("/usr/bin/python", modeldir + "/predict.py", "-e", model, "-a", "-f", fileCmps, "-c", "-v", version)
    println("Cmd: " + cmd.mkString(" "))
    cmd
  }

  lazy val models = (for ((model, tag, iv, ev) <- read_models) yield ((tag + "#" + ev) -> (model, tag, iv, ev))).toMap


  lazy val modelsTag2ModelId = (for ((model, tag, iv, ev) <- read_models) yield (tag -> (model, tag, iv, ev))).toMap

  val read_models = {

    def listDirs(f: File): Array[File] = f.listFiles.filter(_.isDirectory)

    val lf = listDirs(new File(modeldir))

    println("---------")
    val models2 = for (
      f <- lf.map(_.getAbsoluteFile()) if (new File(f.getAbsoluteFile() + "/service-label.txt").exists() && new File(f.getAbsoluteFile() + "/service-version.txt").exists())
    ) yield {
      val slFile = new File(f.getAbsoluteFile() + "/service-label.txt")
      val st = FileUtils.readFileFirstLine(slFile)
      val svVersion = new File(f.getAbsoluteFile() + "/service-version.txt")
      val versions = FileUtils.readFileAll(svVersion)
      (f.getName(), st, versions)
    }

    val models3 = for (
      (modelName, tag, versions) <- models2;
      version <- versions;
      l = version.split("\t");
      internalVersion = l(0);
      externalVersion = l(1) if externalVersion != "0"
    ) yield {
      (modelName, tag, internalVersion, externalVersion)
    }

    models3.filter(_._2.contains("Toxicity"))
  }

  def parseResults(lines: Iterator[String]) = {
    println("Parsing")
    val itype = "quantitative"
    var i = 0
    val resultLines = for (line <- lines) yield {
      println(line)
      val fields = line.split('\t')
      fields.map(println)
      val result = fields.length match {
        case 6 => {
          val valueResult =
            if (fields(0) == "1")
              Map("success" -> "True", "value" -> fields(1))
            else
              Map("success" -> "False", "message" -> fields(1))

          val adResult =
            if (fields(2) == "1")
              Map("success" -> "True", "value" -> fields(3), "message" -> "")
            else
              Map("success" -> "False", "message" -> fields(3))

          val riResult =
            if (fields(4) == "1")
              Map("success" -> "True", "value" -> fields(5), "message" -> "")
            else
              Map("success" -> "False", "message" -> fields(5))

          val jsResult = Json.toJson(valueResult)
          val resResult = jsResult.toString

          val jsAD = Json.toJson(adResult)
          val resAD = "{\"AD\":" + jsAD.toString + "}"

          val jsRI = Json.toJson(riResult)
          val resRI = "{\"RI\":" + jsRI.toString + "}"
          println(resResult)
          println(resAD)
          println(resRI)

          resResult.drop(1).dropRight(1) + "," + resAD.drop(1).dropRight(1) + "," + resRI.drop(1).dropRight(1)
        }
        case _ => {
          val mp = Map("value" -> "", "success" -> "False", "message" -> "unknown error")
          Json.toJson(mp).toString
        }
      }

      val resultLine = "{\"cmp_id\":\"" + i.toString + "\"," + result + "}"
      i += 1
      resultLine
    }
    "[" + resultLines.mkString(",") + "]"
  }

  def parseResults_table(lines: Iterator[String]) = {

    def truncateAt(n: Float, p: Int): Double = {
      val s = math pow(10, p);
      (math floor n * s) / s
    }

    println("Parsing Table")
    val itype = "quantitative"
    var i = 0
    val resultLines = for (line <- lines) yield {
      println(line)
      val fields = line.split('\t')
      fields.map(println)
      val result = fields.length match {
        case 6 => {

          //          val pred_success = if (fields(0) == "1") "True" else "False"
          //          val pred_value = if (fields(0) == "1") fields(1) else ""
          //          val AD_succes = if (fields(2) == "1") "True" else "False"
          //          val AD_value = if (fields(2) == "1") fields(3) else ""
          //          val RI_succes = if (fields(4) == "1") "True" else "False"
          //          val RI_value = if (fields(4) == "1") fields(5) else ""         

          //val pred_success = if (fields(0) == "1") "True" else "False"
          val pred_value = if (fields(0) == "1") fields(1) else "None"
          //val AD_succes = if (fields(2) == "1") "True" else "False"
          val AD_value = if (fields(2) == "1") fields(3) else "None"
          //val RI_succes = if (fields(4) == "1") "True" else "False"
          val RI_value = if (fields(4) == "1") fields(5) else "None"
          val resMap = Map(
            "cmpd_id" -> i.toString,
            "pred_value" -> truncateAt(pred_value.toFloat, 2).toString(),
            "AD_value" -> AD_value,
            "RI_value" -> truncateAt(RI_value.toFloat, 2).toString())
          println(resMap)
          resMap

        }
        case _ => {
          val mp = Map(
            "value" -> "",
            //"pred_success" -> "False",
            "pred_message" -> "unknown error",
            //"AD_succes" -> "",
            "AD_value" -> "",
            //"RI_succes" -> "",
            "RI_value" -> "")
          println(mp)
          mp
        }
      }
      i += 1
      result
    }

    val df = DataFrame(resultLines.toList)
    //val df = resultLines.toList
    df

  }

}