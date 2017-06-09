package model

import java.io.PrintStream

import org.RDKit._

object CompoundUtils {

  def getMolsFromFile(filename: String, importFields: Boolean, sdfAsField: Boolean = false): scala.collection.mutable.LinkedList[Map[String, String]] = {

    def getProps_RDKitMol(m: org.RDKit.ROMol) = {
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
    var i = 0
    var result = scala.collection.mutable.LinkedList[(Int, org.RDKit.ROMol)]()
    while (!molsup.atEnd()) {
      val m = molsup.next()
      result :+= (i, m)
      i = i + 1
    }
    println("Result: " + result.size)
    result
  }

  def getMolsSVG(filename: String) = {
    val l = getMols(filename).map(t => (t._1, getSVGFromMol(t._2)))
    val l2 = l.map(t => Map("id" -> t._1.toString, "structure" -> t._2))
    models.dataframe.DataFrame(l2.toList)
  }

  def openSDFFile(filename: String) = new PrintStream(filename)

  def getSVGFromMol(m: ROMol) = {

    m.compute2DCoords()
    val c = m.getConformer()
    m.WedgeMolBonds(c)
    val drawer = new org.RDKit.MolDraw2DSVG(150, 150)
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