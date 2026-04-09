package org.aprsdroid.app

import _root_.android.content.Context
import _root_.android.util.Log

import java.io.File
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.matching.Regex

// Loads tocalls.yaml and matches a tocall string to a device name.
// The file is parsed once and cached in memory; it reloads automatically
// if the file on disk has changed (e.g. after an update).
object DeviceIdentifier {
  val TAG = "APRSdroid.DeviceIdentifier"

  // Each entry is a compiled pattern paired with its device model name
  private var patterns: Seq[(Regex, String)] = Seq.empty
  private var loadedFileTime: Long = -1

  def tocallsFile(context: Context): File =
    new File(context.getFilesDir, "tocalls.yaml")

  // Returns the device model name for the given tocall, or None if unknown.
  def getDevice(context: Context, tocall: String): Option[String] = {
    if (tocall == null || tocall.isEmpty) {
      Log.d(TAG, "getDevice: empty/null tocall")
      return None
    }
    reloadIfStale(context)
    val result = patterns.find { case (regex, _) => regex.pattern.matcher(tocall).matches() }
                         .map  { case (_, model)  => model }
    Log.d(TAG, "getDevice: tocall='" + tocall + "' patterns=" + patterns.size + " result=" + result.getOrElse("<none>"))
    result
  }

  // Force a reload on the next call to getDevice.
  def invalidate(): Unit = { loadedFileTime = -1 }

  // ---- private helpers ----

  private def reloadIfStale(context: Context): Unit = {
    val file = tocallsFile(context)
    if (!file.exists()) return
    if (file.lastModified() == loadedFileTime) return   // already up to date
    reload(file)
  }

  private def reload(file: File): Unit = {
    Log.i(TAG, "Loading tocalls from " + file.getAbsolutePath)
    val buf = ListBuffer[(Regex, String)]()

    // Parse the YAML conservatively without a full YAML dependency.
    // We only care about the tocalls list entries:
    // tocalls:
    //  - tocall: APXXXX
    //    model: Some Device
    var inTocalls = false
    var currentKey: Option[String] = None

    val lines =
      try   { Source.fromFile(file, "UTF-8").getLines().toSeq }
      catch { case e: Exception => Log.e(TAG, "Failed to read " + file, e); return }

    for (rawLine <- lines) {
      val line = rawLine.replace("\t", "    ")
      val trimmed = line.trim

      if (trimmed == "tocalls:") {
        inTocalls = true
        currentKey = None
      } else if (inTocalls && !trimmed.isEmpty && !trimmed.startsWith("#") && !line.startsWith(" ")) {
        // next top-level section
        inTocalls = false
        currentKey = None
      } else if (inTocalls) {
        if (trimmed.startsWith("- tocall:")) {
          val key = trimmed.substring("- tocall:".length).trim
          if (key.nonEmpty)
            currentKey = Some(key)
        } else if (currentKey.isDefined && trimmed.startsWith("model:")) {
          val model = trimmed.substring("model:".length).trim.stripPrefix("\"").stripSuffix("\"")
          if (model.nonEmpty) {
            buf += ((patternToRegex(currentKey.get), model))
            currentKey = None
          }
        }
      }
    }

    patterns = buf.toSeq
    loadedFileTime = file.lastModified()
    Log.i(TAG, "Loaded %d device patterns".format(patterns.size))
  }

  // Converts a tocall glob pattern (using ? and *) to a Regex.
  // All other regex metacharacters are escaped first.
  private def patternToRegex(pattern: String): Regex = {
    val sb = new StringBuilder
    for (ch <- pattern) ch match {
      case '?' => sb.append('.')
      case '*' => sb.append(".*")
      case c   => sb.append(Regex.quote(c.toString))
    }
    ("(?i)" + sb.toString).r   // case-insensitive: tocalls can be mixed case
  }
}
