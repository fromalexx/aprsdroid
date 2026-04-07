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
    if (tocall == null || tocall.isEmpty) return None
    reloadIfStale(context)
    patterns.find { case (regex, _) => regex.pattern.matcher(tocall).matches() }
            .map  { case (_, model)  => model }
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

    // Simple line-by-line parser for the tocalls section of tocalls.yaml.
    // We only care about the "tocalls:" section and ignore mice/micelegacy.
    var inTocalls = false
    var currentKey: Option[String] = None

    val lines =
      try   { Source.fromFile(file, "UTF-8").getLines().toSeq }
      catch { case e: Exception => Log.e(TAG, "Failed to read " + file, e); return }

    for (line <- lines) {
      // Section headers are at column 0, e.g. "tocalls:"
      if (!line.startsWith(" ") && !line.startsWith("#") && line.endsWith(":")) {
        val section = line.dropRight(1).trim
        inTocalls = (section == "tocalls")
        currentKey = None

      } else if (inTocalls) {
        // A tocall key looks like "  APXXX:" (2-space indent, no leading dash)
        if (line.startsWith("  ") && !line.startsWith("   ") && line.contains(":")) {
          val key = line.trim.dropRight(1)  // strip trailing ":"
          if (key.nonEmpty && !key.startsWith("#") && !key.startsWith("-"))
            currentKey = Some(key)

        // A model line looks like "      model: Some Device Name"
        } else if (currentKey.isDefined && line.contains("model:")) {
          val colonIdx = line.indexOf("model:") + 6
          val model = line.substring(colonIdx).trim
          if (model.nonEmpty) {
            buf += ((patternToRegex(currentKey.get), model))
            currentKey = None   // only take the first model entry per key
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
