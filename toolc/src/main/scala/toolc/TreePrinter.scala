package toolc

object TreePrinter {
  import parser.Trees._

  /** TreePrinter(tree) will produce the same result as before. */
  def apply: (Tree=>String) = apply(false)_

  /** TreePrinter.withSymbolIDs(tree) will print the tree with the IDs. */
  def withSymbolIDs: (Tree=>String) = apply(true)_

  def apply(withSymbolIds: Boolean)(t: Tree): String = {
    /* construct and return the appropriate string ... */
    "Magic magic magic"

    // This adds a symbol id to identifiers, or question marks if it is unset.
    case id @ Identifier(value) => {
          if(withSymbolIDs) {
            try {
              id.getSymbol.name + "#" + id.getSymbol.id
            } catch {
              case e: java.lang.RuntimeException => value + "#??"
            }
          } else value
        }
  }
}
