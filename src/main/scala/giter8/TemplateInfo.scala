package giter8

import java.io.File

case class TemplateInfo(
    defaultProperties: Map[String, String], 
    templates: Stream[File], 
    templatesRoot: File, 
    scaffoldsRoot: Option[File])