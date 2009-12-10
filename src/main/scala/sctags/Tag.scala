package sctags

object Tag {
  def empty = Tag(None, None, None, None, Nil)
}

case class Tag private (_name: Option[String], _kind: Option[String], _file: Option[String], _search: Option[String], _fields: List[(String,String)]) {
  def name = _name
  def kind = _kind
  def search = _search
  def fields = _fields
  def file = _file
  def name_=(s: String) = Tag(Some(s), kind, file, search, fields)
  def kind_=(k: String) = Tag(name, Some(k), file, search, fields)
  def search_=(s: String) = Tag(name, kind, file, Some(s), fields)
  def field_=(k: String, v: String) = Tag(name, kind, file, search, (k,v)::fields)
  def file_=(f: String) = Tag(name, kind, Some(f), kind, fields)

  def isValid = name.isDefined && search.isDefined && file.isDefined
}
