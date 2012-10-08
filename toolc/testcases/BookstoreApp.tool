/* BookstoreApp is written in Toy Object-Oriented Language.
 * It's main feature is to save books, which then can be searched
 * and printed in the terminal.
 * 
 * Author: Michael Schneeberger
 * Date  : 02.10.2012
 */

//Main
object BookstoreApp {
    def main() : Unit = {
	println(new BuildBookstore().go());
    }
}

//Set up a new Bookstore
class BuildBookstore {
    def go() : String = {
	var bookstore : Bookstore;
        var nothing : Int;

	//create new Bookstore
	bookstore = new Bookstore();
	nothing = bookstore.init();

        //------- Add here your book collection ---------
	nothing = bookstore.addBook(409, "Robinson Crusoe");
	nothing = bookstore.addBook(385, "The Count of Monte Christo");
	nothing = bookstore.addBook(589, "Harry Potter");
	nothing = bookstore.addBook(847, "Lord of the Rings");
	//-----------------------------------------------	

	//---------- Add here Books to print ------------
	nothing = bookstore.printBookInfo(385);
	nothing = bookstore.printBookInfo(220);
	nothing = bookstore.printBookInfo(847);
	//-----------------------------------------------

        return "";       
    }	
}

//Bookstore saves a list of books
class Bookstore {
    var lastBook : Book;
    var nullBook : Book;

    //set up bookstore
    def init() : Int = {
	//define empty Book (used to indicate end of list)
	nullBook = new Book();

	lastBook = nullBook;
	return 0;
    }

    //create a book and change its title
    def addBook(id : Int, title : String) : Int = {
	var newBook : Book;
	var nothing : Int;
        
	newBook = new Book();
        nothing = newBook.init(lastBook, id);
	nothing = newBook.changeTitle(title);
	lastBook = newBook;

	return 0;
    }

    //search book with the corresponding id and print informations
    def printBookInfo(id : Int) : Int = {
	var book : Book;
	var nothing : Int;

	book = lastBook.findBook(id, nullBook);
	if (book == nullBook) {
	    println("");
	    println("Book has not been found!");
	} else
	    nothing = book.printBookInfo();
	return 0;
    }
}

//Book saves information about a book
class Book {
    var nextBook : Book;
    var bookId : Int;
    var bookTitle : String;

    //define nextBook which is a link to the next book in the list
    def init(nb : Book, id : Int) : Int = {
	nextBook = nb;
        bookId = id;

	return 0;
    }

    //change title of the book
    def changeTitle(title : String) : Int = {
	bookTitle = title;
	return 0;
    }

    //find book with the corresponding id
    //  nullBook is need to know the end of list
    def findBook(id : Int, nullBook : Book) : Book = {
   	var returnValue : Book;
	
	if (this == nullBook)
	    returnValue = this;
	else
	    if (bookId == id)
            	returnValue = this;
            else
                returnValue = nextBook.findBook(id, nullBook);
        return returnValue;
    }

    def printBookInfo() : Int = {
	println("");
        println("Book ID: " + bookId);
	println("Book Title: " + bookTitle);

	return 0;
    }
}
