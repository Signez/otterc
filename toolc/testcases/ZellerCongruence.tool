// Compiler Construction Course 2012
// Stephane Martin

// Computes and prints the name of any day (as mentioned in ProgramZ.run()) through 
// Zeller's congruence method: 
// http://en.wikipedia.org/wiki/Zeller%27s_congruence
//
// Dates after the 15, 10, 1582 are interpreted as Gregorian dates;
// days falling before are computed in accordance with the Julian
// calendar. (There is no universal date for the beginning of
// the Gregorian era, but the 15 October 1582, following immediately the
// 4 October 1582, is the date mentioned in the bull ``Inter grauissimas'').

object Zeller {
  def main(): Unit = {{
     // first line is the main run, second is a test on n random cases;
     // comment/uncomment as needed.

    if (new ProgramZ().run()){}     
    if (new TestProgZ().runTest(40)){}
  
  }}
}

class ProgramZ {
  def run(): Bool = {   
    var dayNumber: Int;
    var month: Int;
    var year: Int;

    //// The desired date:
    dayNumber = 14;
    month = 2;
    year = 1410;

    return this.compute(dayNumber, month, year);
  }

  def isGregorian(dayNum: Int, month: Int, year: Int): Bool = {
    var julian: Bool;
    if (year < 1582) {
      julian = true;
    } else if (year == 1582 && month < 10) {
      julian = true;
    } else if (year == 1582 && month == 10 && dayNum < 15) {
      julian = true;
    } else {
      julian = false;
    }
    return !julian;
  }

  def compute(dayNumber: Int, month: Int, year: Int): Bool = {   
    var answer: Int;
    
    if (month == 1 || month == 2) {
        answer = this.januaryFebruary(dayNumber, month, year);
    } else {
        answer = this.otherMonths(dayNumber, month, year);
    }
    
    println(this.formatAnswer(dayNumber, month, year, answer));

    return true;
  }

  // Corrects the dates to account for the special case of months 1
  // and 2 (see the Wikipedia link for details).
  def januaryFebruary(dayNum: Int, month: Int, year: Int): Int = {
    var corrMonth: Int;
    var corrYear: Int;
    var answer: Int;
    corrMonth = month + 12;
    corrYear = year - 1;

    if (this.isGregorian(dayNum, month, year)) {
      answer = this.gregorian(dayNum, corrMonth, corrYear); 
    } else {
      answer = this.julian(dayNum, corrMonth, corrYear); 
    }

    return answer;
  }

  def otherMonths(dayNum: Int, month: Int, year: Int): Int = {
    var answer: Int;

    if (this.isGregorian(dayNum, month, year)) {
      answer = this.gregorian(dayNum, month, year); 
    } else {
      answer = this.julian(dayNum, month, year); 
    }  

    return answer;
  }

  // Computes the formula for the Gregorian case, given some date
  // (the date must be already corrected if necessary).
  def gregorian(dayNum: Int, month: Int, year: Int): Int = {
    var h: Int;
    var yearCentury: Int;
    var century: Int;
    yearCentury = this.mod(year, 100);
    century = year / 100;
    h = dayNum + (13 * (month + 1) / 5) + yearCentury + (yearCentury / 4) + (century / 4) - (2 * century);
    return this.mod(h,7);
  }
  
  // Computes the formula for the Julian case, given some date
  // (the date must be already corrected if necessary).
  def julian(dayNum: Int, month: Int, year: Int): Int = {
    var h: Int;
    var yearCentury: Int;
    var century: Int;
    yearCentury = this.mod(year, 100);
    century = year / 100;
    h = dayNum + (13 * (month + 1) / 5) + yearCentury + (yearCentury / 4) + 5 - century;
    return this.mod(h,7);
  }

  def formatAnswer(dayNum: Int, month: Int, year: Int, ans: Int): String = {
    var name: String;
    var answer: String;
    name = this.dayName(ans);
    answer = "The " + dayNum + ", " + month + ", " + year + " is a " + name;
    if (!this.isGregorian(dayNum, month, year)) {
      answer = answer + " (Julian style)";
    }
    answer = answer + ".";
    return answer;
  }

  // Modulo operation (has to work correctly for negative values of num).
  def mod(num: Int, modulus: Int): Int = {
    var nabs: Int;
    var ret: Int;
    if (num < 0) {
      nabs = (0 - 1) * num;  
      ret = ((nabs / modulus) + 1) * modulus - nabs;
    } else {
      ret = num - (num / modulus) * modulus;
    }
    return ret;
  }

  def dayName(dayNum: Int): String = {
    var name: String;
    if (dayNum == 0) {
      name = "Saturday";
    } else if (dayNum == 1) {
      name = "Sunday";
    } else if (dayNum == 2) {
      name = "Monday";
    } else if (dayNum == 3) {
      name = "Tuesday";
    } else if (dayNum == 4) {
      name = "Wednesday";
    } else if (dayNum == 5) {
      name = "Thursday";
    } else if (dayNum == 6) {
      name = "Friday";
    } else {
      name = "Bugday";
    }
    return name;
  }
  
}

// Extends ProgramZ with additional pseudo-random
// methods to create and interpret random dates.
class TestProgZ extends ProgramZ {
  var value: Int;
  var modulus: Int;
  var mult: Int;
  var add: Int;

  def init(): Bool = {
    modulus = 16777216; // 2^31
    mult = 1140671485;
    add = 12820163;
    value = 454213; // seed, change it to get different series
    return true;
  }

  def randomDate(): Int[] = {
    var date: Int[];
    date = new Int[3];
    date[0] = this.mod(this.next(), 7) + 1; // day
    date[1] = this.mod(this.next(), 12) + 1; // month
    date[2] = this.mod(this.next(), 2050) + 1; // year
    return date;
  }

  def next(): Int = {
    value = this.mod((mult * value) + add, modulus);
    return value;
  }

  // Will print the specified number of dates.
  def runTest(nbrDates: Int): Bool = {
    var success: Bool;
    var date: Int[];
    var i: Int;

    success = this.init();
    i = 0;
    while (i < nbrDates) {
      date = this.randomDate();
      success = success && this.compute(date[0], date[1], date[2]);
      i = i + 1;
    }

    return success;
  }

  
}

