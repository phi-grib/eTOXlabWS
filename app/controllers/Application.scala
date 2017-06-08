package controllers

import java.io.File
import java.nio.file.{Files, Path}

import com.typesafe.config.ConfigFactory
import model._
import models.chemistry.CompoundUtil
import models.dataframe.DataFrame
import play.api.libs.json._
import play.api.mvc._

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
    val js = Json.toJson(Map("predictions" -> eTOXlab.read_models.map(
      (tupla: (String, String, String, String)) => Map("model" -> tupla._1, "tag" -> tupla._2, "version" -> tupla._4))))
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
  def getModelInfoVW(tag: String) = Action {
    Ok(views.html.model_info(eTOXvault.getModelInfoMP(tag)))
  }

  // TABLE FROM API RESPONSE

  def storeMol(tmpFile: String) = {
    val numMol = randomGenerator.nextInt.toString
    molecules += (numMol -> tmpFile)
    println("Storing Molecules: ")
    molecules.map(println)
    numMol
  }

  def getPrediction_table = Action(parse.multipartFormData) { request =>

    val form = request.body.asFormUrlEncoded
    request.cookies.toList.map(println)
    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)

    var smilesL = form.get("smiles").toList.flatten
    val smiles = smilesL(0)
    println("SMILES: " + smiles)
    val (fileNameMolecule, idMol) =
      if (smiles != "") {
        println("SMILES case")
        val filename = FileUtils.getTmpFile(tmpDir, ".sdf")
        model.CompoundUtils.getSDFFromSMILES(smiles, filename)
        println("filename: " + filename)
        val idMol = this.storeMol(filename)
        (filename, idMol)
      } else {
        println("SDF case")
        var idMol = request.session.get("molecula").getOrElse("")
        (this.molecules(idMol), idMol)
      }

    // Split tag to get the endpoint name wich is used to call etoxlab prediction: 
    val modelTag = form.get("model").get.head
    println("to get prediction for:" + modelTag)

    val (modelId, _, iv, ev) = eTOXlab.models(modelTag)

    println("Model: " + modelId)

    println("ModelTag:" + modelTag)

    val dfm = CompoundUtils.getMolsSVG(fileNameMolecule)
    val res = eTOXlab.getPredictionDF(modelTag, fileNameMolecule, tmpDir).join(dfm, "cmpd_id", "id")

    println(res.getFields(List()))
    println(dfm.getFields(List()))
    //

    Ok(views.html.home_page("", views.html.mol_info(dfm), views.html.results(res, modelId + " version " + iv), play.api.templates.Html(""))).withSession(("molecula", idMol))
  }

  def upload_Molecule = Action(parse.multipartFormData) { request =>

    val tmpDir: Path = Files.createTempDirectory(FileUtils.tempPath, null)

    val form = request.body.asFormUrlEncoded

    val ufile = request.body.file("uploadfile")
    val ufile2 = ufile.get

    val filename = ufile2.filename
    val contentType = ufile2.contentType

    val tmpFile = FileUtils.getTmpFile(tmpDir, ".sdf")
    ufile2.ref.moveTo(new File(tmpFile), replace = true)

    val dfm = CompoundUtils.getMolsSVG(tmpFile)

    val numMol = this.storeMol(tmpFile)
    println("Uploading Molecule: " + numMol + "/" + tmpFile)
    Ok(views.html.home_page("", views.html.mol_info(dfm), play.api.templates.Html(""), play.api.templates.Html(""))).withSession(("molecula", numMol))
  }

  //url to get model summary json:   http://phi.imim.es/modelinfo/?modeltag=/ADME/Transport/Transporters/ABCB1%20Transport/1&authkey=7b80f381248245c4&provider=UNIVIE

  def homePage = Action {
    Ok(views.html.home_page("", play.api.templates.Html(""), play.api.templates.Html(""), play.api.templates.Html("")))
  }
}
