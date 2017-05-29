package controllers

import java.util.GregorianCalendar
import java.text.SimpleDateFormat
import java.util.Properties
import java.io.FileInputStream
import scala.collection.mutable.HashMap
import scala.collection.immutable.HashSet
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import java.io.File

object FileUtils {
  
  val tempPath: Path = Paths.get(Application.envoy_ws_home + "/temp")
  def getCurrentDirectory = new java.io.File(".").getCanonicalPath
  def getFile(fileName: String) = {
    io.Source.fromFile(fileName).getLines().toSet
  }

  def readPropertiesFile(file: String) = {
    var defaultProps = new Properties()
    var in = new FileInputStream(file)
    defaultProps.load(in)
    in.close()
    defaultProps
  }

  def getNewFilename(prefix: String, suffix: String, path: String) = {
    val calendar = new GregorianCalendar()
    val sdf = new SimpleDateFormat("yyyyMMdd_HHmmss_SSS")
    val datetime = sdf.format(calendar.getTime())
    path + "/" + prefix + "_" + datetime.toString() + suffix
  }

  def readFileFirstLine(file: File) = scala.io.Source.fromFile(file).getLines.take(1).reduceLeft(_ + _)
  def readFileAll(file: File) = scala.io.Source.fromFile(file).getLines.toList

  def getTmpFile(tmpDir: Path, extension: String) =
    {
      val tdir = tmpDir.toAbsolutePath().toString()
      println("Temp!")
      println(tmpDir.toAbsolutePath().toString())
      val tfile = tdir + "/input_file" + extension
      tfile
    }
}