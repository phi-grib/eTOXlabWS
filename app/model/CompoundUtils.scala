package model

import java.io.ByteArrayOutputStream
import java.io.File
import java.nio.file.Files
import java.nio.file.FileSystems
import java.io.PrintStream
import scala.sys.process._
import org.RDKit._
import models.dataframe.DataFrame
import javax.imageio.ImageIO
import org.apache.commons.codec.binary.Base64

object CompoundUtils {

  var sizex = controllers.Application.molsizex
  var sizey = controllers.Application.molsizey

  def getMolsFromFile(filename: String, importFields: Boolean, sdfAsField: Boolean = false): scala.collection.mutable.LinkedList[Map[String, String]] = {

    def getProps_RDKitMol(m: org.RDKit.ROMol) =
      {
        val proplist = m.getPropList(false, false)
        val siz = proplist.size()
        var mp = scala.collection.immutable.Map[String, String]()
        for (i <- Range(0, siz.toInt)) {
          var prop = proplist.get(i)
          if (!prop.startsWith("_")) {
            mp = mp + (prop -> m.getProp(prop).replace('\t', ' '))
          } else {}
        }
        mp
      }

    def getSDF(m: org.RDKit.ROMol) = {
      m.compute2DCoords()
      m.MolToMolBlock()
    }

    val molsup = new org.RDKit.SDMolSupplier(filename, true, true)
    var i = 0
    var result = scala.collection.mutable.LinkedList[Map[String, String]]()
    while (!molsup.atEnd()) {
      val m = molsup.next()
      i = i + 1
      if (m != null) {
        val mp = getProps_RDKitMol(m)
        result :+= mp
      }
    }
    println("Result: " + result.size)
    result
  }

  def getMols(filename: String) = {
    val molsup = new org.RDKit.SDMolSupplier(filename, true, true)
    var i = 1
    var result = scala.collection.mutable.LinkedList[(Int, org.RDKit.ROMol)]()
    while (!molsup.atEnd()) {
      val m = molsup.next()
      result :+= (i, m)
      i = i + 1
    }
    println("Result: " + result.size)
    result
  }

  def getMolsIMG(filename: String) = {
    val l = getMols(filename)
    			.par
    			.map(p => (p._1, getIMGBase64_FromSMiles_RDKit(p._2.MolToSmiles())))
    val img = (base64: String) => "<img alt=\"Embedded Image\" src=\"data:image/png;base64," + base64 + "\"> </img>"
    val l2 = l.map(t => Map("id" -> t._1.toString, "structure" -> img(t._2)))
    models.dataframe.DataFrame(l2.toList)
  }

  def getMolsSVG(filename: String) = {
    val l = getMols(filename).map(t => (t._1, getSVGFromMol(t._2)))
    val l2 = l.map(t => Map("id" -> t._1.toString, "structure" -> t._2))
    models.dataframe.DataFrame(l2.toList)
  }

  def openSDFFile(filename: String) = new PrintStream(filename)

  def getPNGFromSMILES(smiles: String) = {
    val filename = controllers.FileUtils.getNewFilename("img", ".png", controllers.Application.envoy_ws_home + "/temp/img")
    val cmd = "/usr/bin/python " + controllers.Application.envoy_ws_home + "/scripts/generate_img.py " + smiles + " " + filename + " " + sizex + " " + sizey
    println(cmd)
    val cmdProc = Runtime.getRuntime().exec(cmd);
    val retValue = cmdProc.waitFor();
    //println(retValue)
    val f = new File(filename)
    val bi = ImageIO.read(f)
    bi
  }

  def getIMGBase64_FromSMiles_RDKit(smiles: String): String = {
    import sun.misc.BASE64Encoder
    val bi = getPNGFromSMILES(smiles)
    val bos = new ByteArrayOutputStream()
    ImageIO.write(bi, "png", bos)
    val imageBytes = bos.toByteArray()
    val encoder = new BASE64Encoder()
    val imageString = encoder.encode(imageBytes)
    //println("R: " + imageString)
    imageString.toUpperCase()
    imageString
  }

  def getSVGFromMol(m: ROMol) = {

    m.compute2DCoords()
    val c = m.getConformer()
    m.WedgeMolBonds(c)
    val drawer = new org.RDKit.MolDraw2DSVG(100, 100)
    drawer.drawMolecule(m)
    drawer.finishDrawing()
    val svg = drawer.getDrawingText().replace("svg:", "")
    val res = for (l <- svg.split('\n').drop(1)) yield (l)
    res.mkString(" ")
  }
  def getMolFromSmiles(smiles: String) = {
    val m = try {
      val m = org.RDKit.RWMol.MolFromSmiles(smiles)
      m.compute2DCoords()
      m
    } catch {
      case e: Throwable => {
        println("SMILES to SDF failed: " + smiles)
        val m = new org.RDKit.ROMol
        m.compute2DCoords()
        m
      }
    }
    m
  }

  def getSDFFromSMILES(smiles: String, filename: String) = {
    println("SDF filename: " + filename)
    val sw = new org.RDKit.SDWriter(filename)
    println("Converting SMILES to SDF RDKit: " + smiles)
    val m = getMolFromSmiles(smiles)
    sw.write(m)
    sw.flush()
    sw.close()
    //val fileContent = Files.readAllBytes(FileSystems.getDefault().getPath(filename))
    //val file = new File(filename)
    filename
  }

}