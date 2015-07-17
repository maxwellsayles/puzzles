/**
 * After each day, consider the paper value of your portfolio.
 * If you owned nothing yesterday, you can own 'a' or 'b' or nothing today.
 * If you owned 'a' yesterday, you can own nothing or 'a' today.
 * If you owned 'b' yesterday, you can own nothing or 'b' today.
 *
 * a1 = max(z0, a0 + pa1 - pa0)
 * b1 = max(z0, b0 + pb1 - pa0)
 * z1 = max(z0, a1, b1)
 */
var stocks1 = function (pa, pb) {
    var a = 0, b = 0, z = 0;
    for (var i = 1; i < pa.length; i++) {
	a = Math.max(z, a - pa[i-1] + pa[i]);
	b = Math.max(z, b - pb[i-1] + pb[i]);
	z = Math.max(a, b);	
    }
    return z;
}

var stocks2 = function (pa, pb) {
    var opta = [0];
    var optb = [0];
    var opt0 = [0];
    for (var i = 1; i < pa.length; i++) {
	opta[i] = Math.max(opt0[i-1], opta[i-1] - pa[i-1] + pa[i]);
	optb[i] = Math.max(opt0[i-1], optb[i-1] - pb[i-1] + pb[i]);
	opt0[i] = Math.max(opta[i], optb[i]);
    }
    return opt0[pa.length-1];
}

a = [2,8,1,2];
b = [2,1,5,9];
console.log(stocks1(a, b));
console.log(stocksn(a, b))


