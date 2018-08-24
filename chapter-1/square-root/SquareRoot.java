import java.lang.*;

public class SquareRoot {
    public static void main(String[] args) {
	int number = Integer.parseInt(args[0]);
	double guess = 1;
	double quotient = number;

	while(true) {
	    double average = (guess + quotient) / 2;
	    System.out.println("Guess = " + guess + " Quotient = " + quotient + " Average = " + average);
	    if(Math.abs(guess - average) < 0.001D) {
		break;
	    }

	    guess = average;
	    quotient = number / guess;
	}

	System.out.println("Square root = " + guess);
    }
}

