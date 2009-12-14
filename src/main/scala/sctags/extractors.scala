package sctags

import scala.tools.nsc.ast.Trees

object extractors {

  type Extractor = (Trees#Tree,Tag)=>Tag

  def extractor(pf: PartialFunction[Trees#Tree,Tag=>Tag]): Extractor = {
    object idem extends PartialFunction[Trees#Tree,Tag=>Tag] {
      def isDefinedAt(x: Trees#Tree) = true
      def apply(x: Trees#Tree):Tag=>Tag = identity _
    }
    def exec(tree: Trees#Tree, tag: Tag): Tag = {
      pf.orElse(idem)(tree)(tag)
    }
    exec _
  }

  val name = extractor {
    case tree: Trees#DefTree => _.name = tree.name.toString
  }

  val position = extractor {
    case tree => _.search = tree.pos.lineContent
  }

  val kind = extractor {
    case _:Trees#ModuleDef => _.kind = "O"
    case _:Trees#ClassDef => _.kind = "c"
    case _:Trees#TypeDef => _.kind = "t"
    case tree:Trees#ValDef => _.kind = if (tree.mods.isVariable) "v" else "V"
    case _:Trees#DefDef => _.kind = "f"
  }

  def sequence(exs: Seq[Extractor]): Extractor = {
    (tree, tag) => exs.foldLeft(tag)((t,ex)=>ex(tree,t))
  }

  lazy val all = List(name, position, kind)
}
