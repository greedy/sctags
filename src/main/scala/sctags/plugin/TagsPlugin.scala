package sctags
package plugin

import scala.collection._

import scala.tools.nsc
import nsc.Global
import nsc.Phase
import nsc.plugins.{Plugin,PluginComponent}

class TagsPlugin(val global: Global) extends Plugin {
  import global._

  val name = "tags"
  val description = "Creates a tags file for the compilation run"
  val components = List[PluginComponent](MakeTags)

  private object MakeTags extends PluginComponent {
    val global: TagsPlugin.this.global.type = TagsPlugin.this.global
    val runsAfter = List("parser")
    val phaseName = "maketags"
    def newPhase(_prev: Phase) = new MakeTagsPhase(_prev)

    class MakeTagsPhase(prev: Phase) extends StdPhase(prev) {
      override def name = MakeTags.this.phaseName
      def apply(unit: CompilationUnit) {
        val tags = new mutable.ListBuffer[Tag]

        unit.body.foreach { tree =>
          tags += extractors.sequence(extractors.all)(tree, Tag.empty.file = unit.source.path)
        }

        Console.println(tags.filter(_.isValid).mkString("\n"))
      }
    }
  }
}
