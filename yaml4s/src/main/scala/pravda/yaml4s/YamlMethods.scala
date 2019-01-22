/*
 * Copyright (C) 2018  Expload.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package pravda.yaml4s

import java.io.{FileReader, InputStreamReader}

import org.json4s._
import org.yaml.snakeyaml.{DumperOptions, Yaml}

import scala.collection.JavaConverters._
import scala.util.Try

object YamlMethods {

  private val yaml = {
    val doptions = new DumperOptions()
    doptions.setDefaultFlowStyle(DumperOptions.FlowStyle.BLOCK)
    new Yaml(doptions)
  }

  private def jvalue2java(node: JValue): AnyRef = {
    node match {
      case JNull     => null
      case JArray(l) => seqAsJavaList(l.map(jvalue2java))
      case JInt(i) =>
        if (i.isValidInt) Int.box(i.toInt)
        else i.bigInteger
      case JBool(b)    => Boolean.box(b)
      case JDecimal(d) => d
      case JDouble(d)  => Double.box(d)
      case JNothing    => null
      case JString(s)  => s
      case JObject(l)  => mapAsJavaMap(l.toMap.mapValues(jvalue2java))
      case JLong(l)    => Long.box(l)
      case JSet(s)     => setAsJavaSet(s)
    }
  }

  private def java2jvalue(node: AnyRef, ubdfd: Boolean): JValue = node match {
    case null      => JNull
    case s: String => JString(s)
    case l: java.util.List[_] =>
      JArray(l.asScala.toList.map { v =>
        java2jvalue(v.asInstanceOf[AnyRef], ubdfd)
      })
    case m: java.util.Map[_, _] =>
      val pairs = m.asScala.map {
        case (k, v) =>
          (k.asInstanceOf[String], java2jvalue(v.asInstanceOf[AnyRef], ubdfd))
      }.toList
      JObject(pairs)
    case f: java.lang.Double =>
      if (ubdfd) JDecimal(BigDecimal(f))
      else JDouble(f)
    case f: java.lang.Float =>
      if (ubdfd) JDecimal(BigDecimal(f.toDouble))
      else JDouble(f.toDouble)
    case b: java.lang.Boolean    => JBool(b)
    case i: Integer              => JInt(BigInt(i))
    case l: java.lang.Long       => JLong(l)
    case i: java.math.BigInteger => JInt(BigInt(i))
  }

  private def parseUnsafe(in: JsonInput, useBigDecimalForDouble: Boolean): JValue = {
    // WARNING: Yaml.load() accepts a String or an InputStream object.
    // Yaml.load(InputStream stream) detects the encoding by checking the BOM (byte order mark) sequence at the beginning of the stream.
    // If no BOM is present, the utf-8 encoding is assumed.

    val tree = yaml.synchronized {
      in match {
        case StringInput(s)      => yaml.load(s)
        case ReaderInput(rdr)    => yaml.load(rdr)
        case StreamInput(stream) => yaml.load(new InputStreamReader(stream))
        case FileInput(file)     => yaml.load(new FileReader(file))
      }
    }

    java2jvalue(tree, useBigDecimalForDouble)
  }

  private def parseAllUnsafe(in: JsonInput, useBigDecimalForDouble: Boolean): List[JValue] = {
    val trees = yaml.synchronized {
      (in match {
        case StringInput(s)      => yaml.loadAll(s)
        case ReaderInput(rdr)    => yaml.loadAll(rdr)
        case StreamInput(stream) => yaml.loadAll(new InputStreamReader(stream))
        case FileInput(file)     => yaml.loadAll(new FileReader(file))
      }).asScala.toList
    }

    trees.map(t => java2jvalue(t, useBigDecimalForDouble))
  }

  def render(node: JValue): String = {
    val javaObj = jvalue2java(node)
    yaml.synchronized {
      yaml.dump(javaObj)
    }
  }

  /**
    * Render diff of two JValues as yaml.
    * Changed parts are coloured yellow, added parts -- green, deleted parts -- red.
    * The style is fixed, and produced yaml can be wrong, it should be used only in demonstrating purposes.
    *
    * @param node Obtained JValue.
    * @param expected Expected JValue.
    * @return Yaml with colours.
    */
  def renderDiff(node: JValue, expected: JValue): String = {

    def print(j: JValue) = yaml.dump(jvalue2java(j)).trim
    def withColour(s: String, colour: String) = s"$colour$s${Console.RESET}"
    def printWithColour(j: JValue, colour: String) = withColour(print(j), colour)

    def withNewLine(s: String) = if (s.nonEmpty) "\n" + s else s

    def diff(j1: JValue, j2: JValue): String = (j1, j2) match {
      case (x, y) if x == y                     => print(x)
      case (JObject(xs), JObject(ys))           => diffFields(xs, ys)
      case (JArray(xs), JArray(ys))             => diffVals(xs, ys)
      case (x: JInt, y: JInt) if x != y         => printWithColour(y, Console.YELLOW)
      case (x: JDouble, y: JDouble) if x != y   => printWithColour(y, Console.YELLOW)
      case (x: JDecimal, y: JDecimal) if x != y => printWithColour(y, Console.YELLOW)
      case (x: JString, y: JString) if x != y   => printWithColour(y, Console.YELLOW)
      case (x: JBool, y: JBool) if x != y       => printWithColour(y, Console.YELLOW)
      case (JNothing, x)                        => printWithColour(x, Console.GREEN)
      case (x, JNothing)                        => printWithColour(x, Console.RED)
      case (x, y)                               => printWithColour(y, Console.YELLOW)
    }

    def diffFields(xs: List[JField], ys: List[JField]): String = {
      def formatElem(name: String, elem: String) = {
        val lines = elem.lines.toList
        if (lines.length <= 1) {
          s"$name: $elem"
        } else {
          s"""$name:
             |  ${lines.mkString("\n  ")}
           """.stripMargin
        }
      }

      xs match {
        case (xname, xvalue) :: xtail if ys.exists(_._1 == xname) =>
          val (_, yvalue) = ys.find(_._1 == xname).get
          formatElem(xname, diff(xvalue, yvalue)) + withNewLine(diffFields(xtail, ys.filterNot(_ == xname -> yvalue)))
        case (xname, xvalue) :: xtail =>
          withColour(formatElem(xname, print(xvalue)), Console.RED) + withNewLine(diffFields(xtail, ys))
        case Nil =>
          ys match {
            case (yname, yvalue) :: ytail =>
              withColour(formatElem(yname, print(yvalue)), Console.GREEN) + withNewLine(diffFields(Nil, ytail))
            case Nil => ""
          }
      }
    }

    def diffVals(xs: List[JValue], ys: List[JValue]): String = {
      def formatElem(elem: String) = {
        val lines = elem.lines.toList
        lines match {
          case Nil => s"-"
          case head :: tail =>
            val h = s"- $head"
            val t = if (tail.nonEmpty) {
              s"\n${tail.map(s => s"  $s").mkString("\n")}"
            } else {
              ""
            }
            h + t
        }
      }

      (xs, ys) match {
        case (x :: xtail, y :: ytail) => formatElem(diff(x, y)) + withNewLine(diffVals(xtail, ytail))
        case (Nil, y :: ytail)        => withColour(formatElem(print(y)), Console.GREEN) + withNewLine(diffVals(Nil, ytail))
        case (x :: xtail, Nil)        => withColour(formatElem(print(x)), Console.RED) + withNewLine(diffVals(xtail, Nil))
        case (Nil, Nil)               => ""
      }
    }

    diff(node, expected)
  }

  def parseOpt(in: JsonInput, useBigDecimalForDouble: Boolean): Option[JValue] =
    Try { parseUnsafe(in, useBigDecimalForDouble) }.toOption

  def parseAllOpt(in: JsonInput, useBigDecimalForDouble: Boolean): Option[List[JValue]] =
    Try { parseAllUnsafe(in, useBigDecimalForDouble) }.fold(e => { e.printStackTrace(); None }, r => Some(r))
}
