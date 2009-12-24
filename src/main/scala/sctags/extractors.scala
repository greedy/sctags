package sctags

import scala.tools.nsc.ast.Trees
import scala.tools.nsc.symtab.Flags

object extractors {

  def const[A,B](b: B): A=>B = (a:A) => b

  type Extractor = (Trees#Tree,List[Trees#Tree],Tag)=>Tag

  def extractor(pf: PartialFunction[(Trees#Tree,List[Trees#Tree]),Tag=>Tag]): Extractor = {
    object idem extends PartialFunction[(Trees#Tree,List[Trees#Tree]),Tag=>Tag] {
      def isDefinedAt(x: (Trees#Tree,List[Trees#Tree])) = true
      def apply(x: (Trees#Tree,List[Trees#Tree])):Tag=>Tag = identity _
    }
    def exec(tree: Trees#Tree, parents: List[Trees#Tree], tag: Tag): Tag = {
      pf.orElse(idem)(tree,parents)(tag)
    }
    exec _
  }

  val name = extractor {
    case (tree: Trees#DefTree,_) => _.name(tree.name.toString)
  }

  val position = extractor {
    case (tree,_) => if (tree.pos.isDefined) _.line(tree.pos.line) else identity
  }

  val kind = extractor {
    case (_:Trees#ModuleDef,_) => _.kind("O")
    case (_:Trees#ClassDef,_) => _.kind("c")
    case (_:Trees#TypeDef,_) => _.kind("t")
    case (tree:Trees#ValDef,_) => _.kind(if (tree.mods.isVariable) "v" else "V")
    case (_:Trees#DefDef,_) => _.kind("f")
  }

  val memberOf = extractor {
    case (tree:Trees#SymTree,parents) => parents.find(_.isInstanceOf[Trees#ImplDef]) match {
      case Some(klass:Trees#ClassDef) => _.field("class",klass.name.toString)
      case Some(module:Trees#ModuleDef) => _.field("object",module.name.toString)
      case None => identity
    }
  }

  val excludeBogus = extractor {
    case (tree:Trees#DefTree,_) => 
      if (tree.pos.lineContent.contains(tree.name.toString)) {
        identity
      } else {
        const(Tag.empty)
      }
  }

  def sequence(exs: Seq[Extractor]): Extractor = {
    (tree, parents, tag) => exs.foldLeft(tag)((t,ex)=>ex(tree,parents,t))
  }

  lazy val all = List(name, position, kind, memberOf, excludeBogus)
}
