object SqrtApprox {
    def main() : Unit = {
		println(new Util().closestIntOfSqrt(10203));
    }
}

class Util {
	def closestIntOfSqrt(i : Int) : Int = {
		var temp : Int;
		var result : Int;
		var final : Int;
		final = 0;
		result = 2;
		temp = 2;
		if(i == 0) {
			final = 0;
		}
		else {
			if(i < 3) {
				final = 1;
			}
			else {
				if(i==3) {
					final = 2;
				}
				else {
					while (temp < i) {
						result = temp*temp;
						if(result == i) {
							final = temp;
							temp = result; 
						}
						else {
							if(i<result) {
								if((i-(temp-1)*(temp-1))<(result-i)) {
									final = temp - 1;
									temp = result;
								}
								else {
									final = result;
									temp = result;
								}
							}
							else {
								temp = temp + 1;
							}
						}
					}
				}
				
			}

		}
		return final;
	}	
}
