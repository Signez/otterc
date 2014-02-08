package otterc

/**
 * Positioned object.
 *
 * This trait is applied to any object that has a "position" in source file.
 * It will be used in error messages, for example.
 */
trait Positional {
  self =>

  private var _position: Int = -1

  /**
   * Masked integer that represent position in source file.
   *
   * It cannot be printed directly; Reporter methods should be used to do so.
   *
   * @return Masked integer representing position.
   */
  def position: Int = _position

  /**
   * Set position from an integer.
   * 
   * @param position Integer that represent position in file.
   * @return Itself (this method can be chained).
   */
  def setPosition(position: Int): self.type = {
    this._position = position
    self
  }

  /**
   * Copy position from another Positional object.
   *
   * @param other Other Positional object from which position will be copied.
   * @return Itself (this method can be chained).
   */
  def setPosition(other: Positional): self.type = {
    setPosition(other.position)
    self
  }

  /**
   * Human readable position (composed of line and column position)
   * of element.
   *
   * @return String that represent position in a "line:column" form.
   */
  def posString: String = {
    scala.io.Position.line(_position) + ":" + scala.io.Position.column(_position)
  }
}
