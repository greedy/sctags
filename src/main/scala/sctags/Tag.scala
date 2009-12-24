package sctags

object Tag {
  def empty = Tag(None, None, None, None, Nil)
}

case class Tag private (name: Option[String], kind: Option[String], file: Option[String], address: Option[Either[String,Int]], fields: List[(String,String)]) {
  def name(s: String): Tag = Tag(Some(s), kind, file, address, fields)
  def kind(k: String): Tag = Tag(name, Some(k), file, address, fields)
  def search(s: String): Tag = Tag(name, kind, file, Some(Left(s)), fields)
  def line(l: Int): Tag = Tag(name, kind, file, Some(Right(l)), fields)
  def field(k: String, v: String): Tag = Tag(name, kind, file, address, (k,v)::fields)
  def file(f: String): Tag = Tag(name, kind, Some(f), address, fields)

  def isValid = name.isDefined && address.isDefined && file.isDefined
}
