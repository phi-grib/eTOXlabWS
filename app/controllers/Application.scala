package controllers

import play.api._
import play.api.mvc._
import java.io.File
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import scala.io.Source._
import play.api.libs.json._
import play.api.data.Forms
import play.api.data.Form
import scala.collection.mutable.LinkedList

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
  //Startup debug
  println("Models")
  eTOXlab.models.map(println)
  println("ModelsTag to Id")
  eTOXlab.modelsTag2ModelId.map(println)
  //Rest API

  def info = Action {
    val infores = Map("provider" -> "FIMIM", "homepage" -> "http://phi.imim.es", "admin" -> "Manuel Pastor", "admin-email" -> "manuel.pastor@upf.edu")
    val js = Json.toJson(infores)
    Ok(js)
  }

  def getModels = Action {
    val js = Json.toJson((Map("predictions" -> eTOXlab.read_models.map(
      (tupla: (String, String, String, String)) => Map("model" -> tupla._1, "tag" -> tupla._2, "version" -> tupla._4)))))
    Ok(js)
  }

  def getModelInfo(tag: String) = Action {
    Ok(eTOXvault.getModelInfo(tag))
  }

  def getPrediction = Action(parse.multipartFormData) { request =>
    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)
    val form = request.body.asFormUrlEncoded

    val ufile = request.body.file("uploadfile")
    val ufile2 = ufile.get

    val filename = ufile2.filename
    val contentType = ufile2.contentType
    val tmpFile = FileUtils.getTmpFile(tmpDir, ".sdf")
    ufile2.ref.moveTo(new File(tmpFile), replace = true)

    val modelId = form.get("model").get.head

    println("Model: " + modelId)

    val res = eTOXlab.getPredictionJSON(modelId, tmpFile, tmpDir)
    Ok(res).as("application/json")
  }

  // Old GUI

  def predictForm = Action {
    println(".................")
    for (model <- eTOXlab.read_models)
      println(model._1, model._2)
    println(".................")
    Ok(views.html.main("Your new application is ready."))
  }

  // New GUI
  def getModelInfoVW(tag: String) = Action { Ok(views.html.executive_summary(eTOXvault.getModelInfoMP(tag))) }

  // TABLE FROM API RESPONSE

  def getPrediction_table = Action(parse.multipartFormData) { request =>
    request.cookies.toList.map(println)

    //println(request.session.get("molecula"))
    //idMole
    var idMol = request.session.get("molecula").getOrElse("")
    val fileNameMolecule = this.molecules(idMol)

    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)
    val form = request.body.asFormUrlEncoded

    // Split tag to get the endpoint name wich is used to call etoxlab prediction: 
    val modelTag = form.get("model").get.head
    println("to get prediction for:" + modelTag)

    val (modelId, _, iv, ev) = eTOXlab.models(modelTag)

    println("Model: " + modelId)

    println("ModelTag:" + modelTag)
    val res = eTOXlab.getPredictionDF(modelTag, fileNameMolecule, tmpDir)

    val dfm = CompoundUtils.getMolsSVG(fileNameMolecule)
    //val molecules = DataFrame(fileNameMolecule)
    //    val molecules2 = molecules.addRowNum

    Ok(views.html.home2("", views.html.mol_info(dfm), views.html.results(res, modelId + " version " + iv), play.api.templates.Html("")))
  }

  def getMol_infoJSME = Action { request =>

    println("Body: " + request.body)
    val m = request.body.asText.get
    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)
    val tmpFile = FileUtils.getTmpFile(tmpDir, ".sdf")
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

    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)
    println(request.body)

    val form = request.body.asFormUrlEncoded

    val ufile = request.body.file("uploadfile")
    val ufile2 = ufile.get

    val filename = ufile2.filename
    val contentType = ufile2.contentType

    val tmpFile = FileUtils.getTmpFile(tmpDir, ".sdf")
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
