import java.lang.*;
import java.util.*;

public class CountChange {
    public static void main(String[] args) {
        int amount = Integer.parseInt(args[0]);
	SortedSet<Integer> denominations = new TreeSet<Integer>(Arrays.asList(1,5,10,25,50));
	
	System.out.println(countChange(amount, denominations));
    }

    private static long countChange(int amount, SortedSet<Integer> denominations) {
	//reached bottom of recursion tree, thus this is one way to count.
	if(amount == 0)
	    return 1L;

	//amount is too low so this is not a way to count
	//or no more coins are left
	if(amount < 0 || denominations.isEmpty())
	    return 0L;

	int highestCoin = denominations.last();
	//Don't use highest denomination try other coins; and dont include highest coin to avoid duplicate ways.
	return countChange(amount, denominations.headSet(highestCoin))
	    //Use the highest denomination and countChange for leftover amount using all the coins.
	    + countChange(amount - highestCoin, denominations);
    }
}

