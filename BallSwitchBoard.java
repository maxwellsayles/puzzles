/**
 * Taken from http://codility.com on May 9, 2011.
 *
 * Given a switch board, where the values can be -1, 0, +1,
 * we drop a ball in the top-left.  If the value is 0, the 
 * ball passes through, if the value is -1, the ball leaves
 * through the bottom, and if the value is +1, the ball leaves
 * through the right side.  Each time a ball passes through a
 * cell, the value is negated.
 *
 * Given k balls, how many pass through the switchboard and
 * out the bottom of the bottom-right cell.
 *
 *
 * The solution given here runs in O(n*m) where n is the width
 * and m is the height of the switchboard.
 *
 * Each cell computes the number of balls that enter from above
 * and from the left.  We iterate from the top-left to the
 * bottom-right computing this function.  This gives us the final value.
 */

class Solution {
    int[][] A;
    int[] b;
    int[] r;
    int k;
    
    /**
     * computes the number of balls to make it into this square
     */
    void calc(int x, int y) {
	int left = 0; // balls from left
	int top = 0; // balls from top
	
	if (x == 0 && y == 0) {
	    // top, left
	    top = k;
	}
	
	if (x > 0) {
	    // count balls coming from the left
	    int t = r[x-1] + b[x-1];
	    switch (A[y][x-1]) {
	    case -1:
		// cell to the left goes down first
		left = t / 2;
		break;
	    case 0:
		// cell to the left passes through
		left = r[x-1];
		break;
	    case 1:
		// cell to the left goes right first
		left = (t + 1) / 2; 
		break;
	    } 
	}
	
	if (y > 0) {
	    // count balls coming from the top
	    int t = r[x] + b[x];
	    switch (A[y-1][x]) {
	    case -1:
		// cell above goes down first
		top = (t + 1) / 2;
		break;
	    case 0:
		// cell above passes through
		top = b[x];
		break;
	    case 1:
		// cell above goes right first
		top = t / 2;
		break;
	    }
	}
	
	// store results
	b[x] = top;
	r[x] = left;
    }
    
    
    /**
     * -1 => ball leaves through the bottom
     * +1 => ball leaves through the right
     */
    int ball_switch_board(int[][] A, int k) {
	this.A = A;
	this.k = k;
	
	int height = A.length;
	if (height == 0) {
	    return k;
	}
	int width = A[0].length;
	
	this.b = new int[width];
	this.r = new int[width];
	
	// iterate over each cell
	for (int y = 0; y < height; y++) {
	    for (int x = 0; x < width; x ++) {
		calc(x, y);
	    }
	}
	
	// given the lower-right, determine how many pass down
	int t = b[width-1] + r[width-1];
	switch (A[height-1][width-1]) {
	case -1:
	    return (t + 1) / 2;
	case 0:
	    return b[width-1];
	case 1:
	    return t / 2;
	}
	
	return 0;
    }
}

public class BallSwitchBoard {
    public static void main(String[] args) {
	int[][] A = new int[2][3];
	A[0][0] = -1;
	A[0][1] =  0;
	A[0][2] = -1;
	A[1][0] = +1;
	A[1][1] =  0;
	A[1][2] =  0;
	Solution s = new Solution();
	System.out.println(s.ball_switch_board(A, 4));
    }
}
