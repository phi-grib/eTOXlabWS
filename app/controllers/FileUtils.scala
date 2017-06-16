package controllers

import java.io.{File, FileInputStream}
import java.nio.file.{Path, Paths}
import java.text.SimpleDateFormat
import java.util.{GregorianCalendar, Properties}
object Lib {
  def loadLibs={
    System.load("/opt/envoy_ws/lib/libboost_thread.so.1.56.0")
    System.load("/opt/envoy_ws/lib/libboost_system.so.1.56.0")
    System.load("/opt/envoy_ws/lib/libGraphMolWrap.so")

  }
}
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
    path + "/" + prefix + "_" + datetime.toString()  +"_"+controllers.Application.randomGenerator.nextInt.toString + suffix
  }

  def readFileFirstLine(file: File) = scala.io.Source.fromFile(file).getLines.take(1).reduceLeft(_ + _)

  def readFileAll(file: File) = scala.io.Source.fromFile(file).getLines.toList

  def getTmpFile(tmpDir: Path, extension: String) = {
    val tdir = tmpDir.toAbsolutePath().toString()
    println("Temp!")
    println(tmpDir.toAbsolutePath().toString())
    val tfile = tdir + "/input_file" + extension
    tfile
  }
}