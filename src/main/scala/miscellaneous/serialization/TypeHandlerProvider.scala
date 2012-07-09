package miscellaneous.serialization

trait TypeHandlerProvider {
  def handlers: Seq[TypeHandler]
}