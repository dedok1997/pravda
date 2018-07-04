package pravda.yopt.printers

import pravda.yopt.CommandLine
import pravda.yopt.CommandLine.{Cmd, CmdPath, Opt}

import sys.process._
import scala.collection.mutable.ListBuffer
import scala.util.Try

object ConsolePrinter {
  private val EOL = sys.props("line.separator")
  private val shiftSize = 2
  private val maxWidth = Try { "tput cols".!!.trim.toInt }.getOrElse(80)

  private def wrapToLines(text: String, maxWidth: Int): List[String] = {
    val lines = text.split("\n").toList.map(_.split("\\s+").toList)
    val res = ListBuffer[List[String]]()
    val buffer = ListBuffer[String]()
    var curWidth = 0

    def nextLine(): Unit = {
      res += buffer.toList
      buffer.clear()
      curWidth = -1
    }

    def addWord(word: String): Unit = {
      buffer += word
      curWidth += word.length + 1
    }

    lines.foreach(line => {
      line.foreach { word =>
        if (curWidth + word.length + 1 > maxWidth) {
          nextLine()
        }
        addWord(word)
      }
      nextLine()
    })

    if (buffer.nonEmpty) {
      nextLine()
    }
    res.toList.map(_.mkString(" "))
  }

  private def padWithoutFirstLine(lines: List[String], pad: Int): String =
    (lines.head :: lines.tail.map(" " * pad + _)).mkString("\n")

  def printPath[C](cmdPath: CmdPath[C], shift: Int = 2): String = {
    val cmds = cmdPath.verbs.collect { case x: Cmd[C] => x }
    val opts = Opt[C, Unit](Some('h'), "help", "Print this help message") :: cmdPath.opts
    val hasSubSubCommands = cmds.exists(_.verbs.exists(_.isInstanceOf[Cmd[_]]))

    def maxOrElse(seq: Seq[Int], default: => Int): Int = if (seq.nonEmpty) seq.max else default
    def maxPad(cmd: Cmd[C]): Int =
      math.max(cmd.name.length, shiftSize + maxOrElse(cmd.verbs.collect { case c: Cmd[C] => maxPad(c) }, 0))

    val cmdsPad = shift + maxOrElse(cmds.map(maxPad), 0) + shiftSize
    val optsPad = shift + maxOrElse(opts.map(o => o.short.fold(0)(_ => 4) + 2 + o.name.length), 0) + shiftSize

    val cmdP = cmds.map(printCmd(_, cmdsPad, shift, hasSubSubCommands)).mkString
    val optP = opts.map(printOpt(_, optsPad, shift)).mkString(EOL)

    val usageText = wrapToLines(cmdPath.toUsageString, maxWidth).mkString("\n")
    val text = wrapToLines(cmdPath.text, maxWidth).mkString("\n")
    val optText = if (opts.isEmpty) "" else s"Options:$EOL$optP$EOL"
    val cmdText = if (cmds.isEmpty) "" else s"Commands:$EOL$cmdP"
    s"$usageText$EOL$EOL$text$EOL$EOL$optText$cmdText"
  }

  private def printCmd[C](cmd: CommandLine.Cmd[C], pad: Int, shift: Int, divideCmds: Boolean): String = {
    val body = printNestedCmds(cmd.verbs, pad, shift + shiftSize) + (if (divideCmds) EOL else "")
    val text = padWithoutFirstLine(wrapToLines(cmd.text, maxWidth - pad), pad)
    s"${" " * shift}%-${pad - shift}s$text$EOL$body".format(cmd.name)
  }

  private def printNestedCmds[C](verbs: List[CommandLine.Verb[C]], pad: Int, shift: Int): String =
    verbs.collect { case x: Cmd[C] => x }.map(printCmd(_, pad, shift, false)).mkString

  private def printOpt[C](opt: CommandLine.Opt[C, _], pad: Int, shift: Int): String = {
    val optTextLines = padWithoutFirstLine(wrapToLines(opt.text, maxWidth - pad), pad)
    s"${" " * shift}%-${pad - shift}s$optTextLines"
      .format(s"${opt.short.map(x => s"-$x, ").getOrElse("")}--${opt.name}")
  }
}
