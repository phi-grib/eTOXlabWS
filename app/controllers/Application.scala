package controllers

import play.api._
import play.api.mvc._
import java.io.File
import scala.io.Source._
import play.api.libs.json._
import play.api.data.Forms
import play.api.data.Form
import scala.sys.process._
import scala.collection.mutable.LinkedList
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import models.dataframe._
import models.dataframe.DataFrame
import java.io._
import java.nio.charset.StandardCharsets
import models.dataframe.DataFrame
import models.dataframe._
import models.chemistry.CompoundUtil
import model._
import play.api.libs.ws._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration._
import models.dataframe.DataFrame
import java.net.URLEncoder

object Application extends Controller {

  val randomGenerator = scala.util.Random
  var molecules: Map[String, String] = Map()

  // per carregar la llibreria binaria de RDKit

  CompoundUtil.getSDFFromSMILES("CC")

  val conf = ConfigFactory.load()
  val etoxlabhome = conf.getString("etoxlabhome")
  val modeldir = etoxlabhome + "/src"
  val envoy_ws_home = conf.getString("envoy_ws_home")
  val tempPath: Path = Paths.get(envoy_ws_home + "/temp")

  def readFileFirstLine(file: File) = scala.io.Source.fromFile(file).getLines.take(1).reduceLeft(_ + _)
  def readFileAll(file: File) = scala.io.Source.fromFile(file).getLines.toList

  def callPredictEtoxLab(model: String, version: String, fileCmps: String) = {
    val cmd = Seq("/usr/bin/python", modeldir + "/predict.py", "-e", model, "-a", "-f", fileCmps, "-c", "-v", version)
    println("Cmd: " + cmd.mkString(" "))
    cmd
  }

  def getTmpFile(tmpDir: Path, extension: String) =
    {
      val tdir = tmpDir.toAbsolutePath().toString()
      println("Temp!")
      println(tmpDir.toAbsolutePath().toString())
      val tfile = tdir + "/input_file" + extension
      tfile
    }

  lazy val models = (for ((model, tag, iv, ev) <- read_models) yield ((tag + "#" + ev) -> (model, tag, iv, ev))).toMap

  lazy val modelsTag2ModelId = (for ((model, tag, iv, ev) <- read_models) yield (tag -> (model, tag, iv, ev))).toMap

  lazy val read_models =
    {
      println("Read Models")

      def listDirs(f: File): Array[File] = f.listFiles.filter(_.isDirectory)

      println(modeldir)
      val lf = listDirs(new File(modeldir))

      println("---------")
      val models2 = for (
        f <- lf.map(_.getAbsoluteFile()) if (new File(f.getAbsoluteFile() + "/service-label.txt").exists() && new File(f.getAbsoluteFile() + "/service-version.txt").exists())
      ) yield ({
        val slFile = new File(f.getAbsoluteFile() + "/service-label.txt")
        val st = readFileFirstLine(slFile)
        val svVersion = new File(f.getAbsoluteFile() + "/service-version.txt")
        val versions = readFileAll(svVersion)
        (f.getName(), st, versions)
      })

      val models3 = for ((modelName, tag, versions) <- models2; version <- versions; l = version.split("\t"); internalVersion = l(0); externalVersion = l(1) if externalVersion != "0")
        yield ({
        println(modelName + "#" + tag)
        println("V: " + internalVersion + "#" + externalVersion)
        (modelName, tag, internalVersion, externalVersion)
      })

      models3
    }

  def info = Action {
    val infores = Map("provider" -> "FIMIM", "homepage" -> "http://phi.imim.es", "admin" -> "Manuel Pastor", "admin-email" -> "manuel.pastor@upf.edu")
    val js = Json.toJson(infores)
    Ok(js)
  }

  def getModels = Action {
    val js = Json.toJson((Map("predictions" -> read_models.map(
      (tupla: (String, String, String, String)) => Map("model" -> tupla._1, "tag" -> tupla._2, "version" -> tupla._4)))))
    Ok(js)
  }
  def predictForm = Action {
    println(".................")
    for (model <- Application.read_models)
      println(model._1, model._2)
    println(".................")
    Ok(views.html.main("Your new application is ready."))
  }
  def getCurrentDirectory = new java.io.File(".").getCanonicalPath

  def parseResults(file: String) = {
    println("Parsing")
    val itype = "quantitative"
    var i = 0
    val resultLines = for (line <- scala.io.Source.fromFile(file).getLines) yield ({
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
          Future
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
    })
    "[" + resultLines.mkString(",") + "]"
  }

  def getPrediction = Action(parse.multipartFormData) { request =>
    val tmpDir: Path = Files.createTempDirectory(tempPath, null)
    val form = request.body.asFormUrlEncoded

    val ufile = request.body.file("uploadfile")
    val ufile2 = ufile.get

    val filename = ufile2.filename
    val contentType = ufile2.contentType
    val tmpFile = getTmpFile(tmpDir, ".sdf")
    ufile2.ref.moveTo(new File(tmpFile), replace = true)

    val modelId = form.get("model").get.head

    val f = new File(tmpDir.toAbsolutePath().toString())
    println("Model tag: " + modelId)
    val (model, tag, iv, ev) = models(modelId)

    val p = Process(callPredictEtoxLab(model, iv, tmpFile), cwd = Some(f))

    println(callPredictEtoxLab(tag, iv, tmpFile))
    val q = p.!

    val res = parseResults(tmpDir.toAbsolutePath().toString() + "/result.txt")
    println(tmpDir.toAbsolutePath().toString() + "/result.txt")
    //val p2 = Seq("rm", "-Rf", tmpDir.toAbsolutePath().toString())
    //p2!

    Ok(res).as("application/json")
  }

  // TABLE FROM API RESPONSE

  def parseResults_table(file: String) = {
    println("Parsing Table")
    val itype = "quantitative"
    var i = 0
    val resultLines = for (line <- scala.io.Source.fromFile(file).getLines) yield ({
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
            "pred_value" -> pred_value,
            "AD_value" -> AD_value,
            "RI_value" -> RI_value)
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
    })

    val df = DataFrame(resultLines.toList)
    //val df = resultLines.toList
    df

  }

  // old one. not working >>>>>

  //  def getPrediction_table = Action(parse.multipartFormData) { request =>
  //
  //    val tmpDir: Path = Files.createTempDirectory(tempPath, null)
  //    val form = request.body.asFormUrlEncoded
  //
  //    val ufile = request.body.file("uploadfile")
  //    val ufile2 = ufile.get
  //
  //    val filename = ufile2.filename
  //    val contentType = ufile2.contentType
  //    val tmpFile = getTmpFile(tmpDir, ".sdf")
  //    ufile2.ref.moveTo(new File(tmpFile), replace = true)
  //
  //    val modelId = form.get("model").get.head
  //
  //    val f = new File(tmpDir.toAbsolutePath().toString())
  //    println("Model tag: " + modelId)
  //    val (model, tag, iv, ev) = models(modelId)
  //
  //    val p = Process(callPredictEtoxLab(model, iv, tmpFile), cwd = Some(f))
  //
  //    println(callPredictEtoxLab(tag, iv, tmpFile))
  //    val q = p.!
  //
  //    val res = parseResults_table(tmpDir.toAbsolutePath().toString() + "/result.txt")
  //    println(tmpDir.toAbsolutePath().toString() + "/result.txt")
  //
  //    val dfm = CompoundUtils.getMolsSVG(tmpFile)
  //    val dfr = res.join(dfm, "cmpd_id", "id")
  //    println(dfm)
  //    println(dfr)
  //
  //    val molecules = DataFrame(tmpFile)
  //    val molecules2 = molecules.addRowNum
  //
  //    //val molecules3 = molecules2
  //      //.join(dfr, "rownum", "cmpd_id")
  //      //.dropFields(List("rownum", "id", "AuxInfo", "StdInChI", "StdInChIKey"))
  //      //.project("name","svg")
  //      
  //
  //    Ok(views.html.homepage("", views.html.results(res)))
  //  }

  def getPrediction_table = Action(parse.multipartFormData) { request =>
    request.cookies.toList.map(println)

    //println(request.session.get("molecula"))
    //idMole
    var idMol = request.session.get("molecula").getOrElse("")
    val fileNameMolecule = this.molecules(idMol)

    val tmpDir: Path = Files.createTempDirectory(tempPath, null)
    val form = request.body.asFormUrlEncoded

    //    val ufile = request.body.file("uploadfile")
    //    val ufile2 = ufile.get
    //
    //    val filename = ufile2.filename
    //    val contentType = ufile2.contentType
    //    val tmpFile = getTmpFile(tmpDir, ".sdf")
    //    ufile2.ref.moveTo(new File(tmpFile), replace = true)

    // Split tag to get the endpoint name wich is used to call etoxlab prediction: 
    val modelTag = form.get("model").get.head

    val (modelId, _, iv, ev) = modelsTag2ModelId(modelTag)

    val f = new File(tmpDir.toAbsolutePath().toString())

    val p = Process(callPredictEtoxLab(modelId, iv, fileNameMolecule), cwd = Some(f)) //changed "model" with "tag"
    //println(callPredictEtoxLab(tag, iv, fileNameMolecule))
    val q = p.!
    //println("eTOX lab return: " + q) // sempre retorna 0 (fix etox)

    val res = parseResults_table(tmpDir.toAbsolutePath().toString() + "/result.txt") //return prediction result
    println(tmpDir.toAbsolutePath().toString() + "/result.txt")

    val dfm = CompoundUtils.getMolsSVG(fileNameMolecule) // molecule information 
    // val mol_info = dfm.dropFields(List("rownum", "AuxInfo", "StdInChI", "StdInChIKey"))

    val molecules = DataFrame(fileNameMolecule)
    val molecules2 = molecules.addRowNum

    //val molecules3 = molecules2
    //.join(dfr, "rownum", "cmpd_id")
    //.dropFields(List("rownum", "id", "AuxInfo", "StdInChI", "StdInChIKey"))
    //.project("name","svg")

    Ok(views.html.home2("", views.html.mol_info(dfm), views.html.results(res), play.api.templates.Html("")))
  }

  // old one. not working >>>>>

  //  def getModelInfo = {
  //    val model_json_url = "http://phi.imim.es/modelinfo/?modeltag=/ADME/Transport/Transporters/ABCB1%20Transport/1&authkey=7b80f381248245c4&provider=UNIVIE"
  //    implicit val context = scala.concurrent.ExecutionContext.Implicits.global
  //    var m: scala.collection.Map[String, String] = null
  //    val futureResult = WS.url(model_json_url).get().map {
  //      response =>
  //        val jso = response.json.as[JsObject]
  //        val mp = for ((field, value) <- jso.value)
  //          yield ((field, value.toString))
  //        println(mp)
  //        m = mp
  //        mp
  //    }
  //    m
  //  }

  def getModelInfo(tag: String) = Action {
    //println("Model info: " + tag)

    val model_json_url = "http://lamia.upf.edu/modelinfo/?modeltag=" + tag + "&authkey=7b80f381248245c4&provider=FIMIM"
    implicit val context = scala.concurrent.ExecutionContext.Implicits.global
    //var m: scala.collection.Map[String, String] = null
    //println(model_json_url)
    val futureResult_url = WS
      .url(model_json_url)
    //.withQueryString("modeltag"-> tag, "authkey"->"7b80f381248245c4", "provider"->"FIMIM")

    val futureResult = futureResult_url.get()
    val response = Await.result(futureResult, 10 seconds)
    //println("ATENTION:  " + response.body)
    val jso = Json.parse(response.body).as[JsObject]
    val mp = for ((field, value) <- jso.value if (field != "software"))
      yield ((field, value.as[JsString].value))
    //println(mp)
    //Ok(result)    

    Ok(views.html.executive_summary(mp))
  }

  def getMol_infoJSME = Action { request =>

    println("Body: " + request.body)
    val m = request.body.asText.get
    val tmpDir: Path = Files.createTempDirectory(tempPath, null)
    val tmpFile = getTmpFile(tmpDir, ".sdf")
    import java.nio.file.{ Paths, Files }
    import java.nio.charset.StandardCharsets
    println(tmpFile)
    Files.write(Paths.get(tmpFile), m.getBytes(StandardCharsets.UTF_8))

    val dfm = CompoundUtils.getMolsSVG(tmpFile)
    //val mol_info = dfm.dropFields(List("rownum", "AuxInfo", "StdInChI", "StdInChIKey"))
    println("dATAfRAME: " + dfm)
    val numMol = randomGenerator.nextInt
    this.molecules += (numMol.toString -> tmpFile)

    Ok(views.html.home2("", views.html.mol_info(dfm), play.api.templates.Html(""), play.api.templates.Html(""))).withSession(("molecula", numMol.toString))
  }

  def getMol_info = Action(parse.multipartFormData) { request =>

    val tmpDir: Path = Files.createTempDirectory(tempPath, null)
    println(request.body)

    val form = request.body.asFormUrlEncoded

    val ufile = request.body.file("uploadfile")
    val ufile2 = ufile.get

    val filename = ufile2.filename
    val contentType = ufile2.contentType

    val tmpFile = getTmpFile(tmpDir, ".sdf")
    ufile2.ref.moveTo(new File(tmpFile), replace = true)

    val dfm = CompoundUtils.getMolsSVG(tmpFile)
    //val mol_info = dfm.dropFields(List("rownum", "AuxInfo", "StdInChI", "StdInChIKey"))

    val numMol = randomGenerator.nextInt
    this.molecules += (numMol.toString -> tmpFile)

    Ok(views.html.home2("", views.html.mol_info(dfm), play.api.templates.Html(""), play.api.templates.Html(""))).withSession(("molecula", numMol.toString))
  }

  //url to get model summary json:   http://phi.imim.es/modelinfo/?modeltag=/ADME/Transport/Transporters/ABCB1%20Transport/1&authkey=7b80f381248245c4&provider=UNIVIE

  def homePage = Action {
    val dt = DataFrame(List())
    Ok(views.html.home2("", play.api.templates.Html(""), play.api.templates.Html(""), play.api.templates.Html("")))
  }
}
