package compiler.generator.utils

import compiler.generator.utils.GeneratorUtils._

import scala.language.postfixOps

object ASMFileWriter {
  // write assembly instructions to specified file
  def writeToFile(fileName: String, as: AssemblyState): Unit = {
    import java.io._

    // Create new file
    val file = new File(s"$fileName.s")
    // Instantiate writer
    val writer = new PrintWriter(file)
    // Write instructions
    writer.write(".data" + NEWLINE + NEWLINE)
    for ((rawString, label) <- as.dataSection) {
      writer.write(s"$label:$NEWLINE$INDENT.word ${rawString.length}$NEWLINE$INDENT" +
        s".ascii \"${evalRawString(rawString)}\"$NEWLINE$NEWLINE")
    }
    writer.write(".text" + NEWLINE + NEWLINE)

    writer.write(NEWLINE + ".global main" + NEWLINE + NEWLINE)
    for (proc <- as.codeSection) {
      writer.write(proc.toString)
    }
    // Done writing, close writer
    writer.close()
  }

  // For replacing escape chars with the actual representation of escape chars
  // For example \n => \\n
  private def doubleEscape(s: String): String = {
    val regexSlash = "\\\\"
    regexSlash + (s match {
      case "\n" =>  "n"
      case "\"" => "\""
      case "'" => "'"
      case "\b" => "b"
      case "\r" => "r"
      case "\t" => "t"
      case "\f" => "f"
      case "\\" => regexSlash
      case "\u0000" => "0"
    })
  }

  def evalRawString(rawString: String): String = {
    val escapeCharRegex = "\n|\"|\'|\b|\r|\t|\f|\\\\|\u0000"
    escapeCharRegex.r.replaceAllIn(rawString, m => doubleEscape(m.group(0)))
  }

}
