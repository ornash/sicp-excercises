import java.lang.*;

/**
 * Exercise 1.11, Page 42
 */

public class FOneEleven {
    public static void main(String[] args) {
	int n = Integer.parseInt(args[0]);
	
        System.out.println(recurse(n));
    }

    public static long
	recurse(int n) {
	if(n < 3)
	    return n;
	else {
	    return recurse(n-1)
		+ 2 * recurse(n-2)
		+ 3 * recurse(n-3);
	}
    }
}

