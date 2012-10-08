object BinaryTree {
    def main() : Unit = { 
      if(new Tree().init()) {  }
    }
}

class Node {
  var left: Node;
  var right: Node;
  var value: Int; 
  var leftEmpty: Bool;
  var rightEmpty: Bool; 
  var tempString: String;
  
  def init(val: Int): Node = { 
    leftEmpty = true;
    rightEmpty = true;
    value = val;           
    return this;
  }                          
  
  def getValue(): Int = { return value; } 
  def getLeft(): Node = { return left; }
  def getRight(): Node = { return right; } 
  
  def setValue(val: Int): Bool = { value = val; return true; } 
  def setLeft(node: Node): Bool = { left = node; return true; }
  def setRight(node: Node): Bool = { right = node; return true; } 
  def setLeftEmpty(b: Bool): Bool = { leftEmpty = b; return true; }
  def setRightEmpty(b: Bool): Bool = { rightEmpty = b; return true; }
  
  def leftEmpty(): Bool = { return leftEmpty; }
  def rightEmpty(): Bool = { return rightEmpty; }
  
} 

class Tree {   
  var root: Node;
  var voidCall: Bool; 
  var tempInt: Int;   
  var tempString: String;
  
  def init(): Bool = {
    root = new Node().init(17);                   
    voidCall = this.insert(root, 11); 
    voidCall = this.insert(root, 20);
    voidCall = this.insert(root, 3);              
    voidCall = this.insert(root, 7);               
    voidCall = this.insert(root, 21);   
    voidCall = this.insert(root, 19);   
    
    println("Tree has depth " + this.depth(root));  
    
    println("Preorder traversal: " + this.printPreOrder(root));        
    println("Inorder traversal: " + this.printInOrder(root));        
    println("Postorder traversal: " + this.printPostOrder(root));        

    return true;
  }
  
  def insert(node: Node, val: Int): Bool = {
    if (val < node.getValue()) {
      if (!node.leftEmpty()) {
        voidCall = this.insert(node.getLeft(), val);
      } else { 
        voidCall = node.setLeftEmpty(false);    
        voidCall = node.setLeft(new Node().init(val));
        println("Inserted " + val + " to left of node " + node.getValue());
      }
    } 
    else {
      if (!node.rightEmpty()) {
        voidCall = this.insert(node.getRight(), val);
      } else {
        voidCall = node.setRightEmpty(false);             
        voidCall = node.setRight(new Node().init(val));
        println("Inserted " + val + " to right of node " + node.getValue());
      }
    }
    return true;
  }
	
	def printPreOrder(node: Node): String = {  
    tempString = "";
    tempString = tempString + node.getValue() + " ";
    if (!node.leftEmpty()) { tempString = tempString + this.printPreOrder(node.getLeft()); }  
    if (!node.rightEmpty()) { tempString = tempString + this.printPreOrder(node.getRight()); }
    return tempString;
	} 
	
  def printInOrder(node: Node): String = {  
    tempString = "";
    if (!node.leftEmpty()) { tempString = tempString + this.printInOrder(node.getLeft()); }  
    tempString = tempString + node.getValue() + " ";
    if (!node.rightEmpty()) { tempString = tempString + this.printInOrder(node.getRight()); }
    return tempString;
	}
		
	def printPostOrder(node: Node): String = {  
    tempString = "";
    if (!node.leftEmpty()) { tempString = tempString + this.printPostOrder(node.getLeft()); }  
    if (!node.rightEmpty()) { tempString = tempString + this.printPostOrder(node.getRight()); }
    tempString = tempString + node.getValue() + " ";
    return tempString;
	}
  
  def max(a: Int, b: Int): Int = {
    if (a < b) { tempInt = b; } else { tempInt = a; }
    return tempInt;
  }
  
  def depth(node: Node): Int = {     
    tempInt = 0;
    if (node.leftEmpty()) {
      if (node.rightEmpty()) { tempInt = 1; } 
      else { tempInt = 1 + this.depth(node.getRight()); }
    } else {
      if (node.rightEmpty()) { tempInt = 1 + this.depth(node.getLeft()); }
      else { tempInt = 1 + this.max(this.depth(node.getLeft()), this.depth(node.getRight())); }
    }                
    return tempInt;
  } 
  

}