package toolc

/** This trait applies to every object to which a corresponding position
 * in the input source file can be associated. This information is useful
 * in error messages, for instance. */
trait Positional {
  self =>
  
  private var _pos: Int = -1

  def pos: Int = _pos
  
  def setPos(pos: Int): self.type = {
    this._pos = pos
    self
  }

  /** Copies the position from another Positional object. Returns the
   * object on which the setPos method was called. */
  def setPos(other: Positional): self.type = {
    setPos(other.pos)
    self
  }

  /** Returns a string in the form row:col. */
  def posString: String = {
    scala.io.Position.line(_pos) + ":" + scala.io.Position.column(_pos)
  }
}
