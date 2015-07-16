var stocks = function (pa, pb) {
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

console.log(stocks([2,8,1,2],[2,1,5,9]));

