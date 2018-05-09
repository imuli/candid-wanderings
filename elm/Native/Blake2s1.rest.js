var _imuli$candid$Native_Blake2s1 = {
	hash:	(left) => (right) =>
		(w) => (x) => (y) => (z) =>
		blake2s1.hash(left.concat(right), [w,x,y,z], []),
	toHex:	blake2s1.toHex,
	index:	(h) => (i) => h[i] | 0
	toUni:	(h) => {
			var a = [];
			for(var i = 0; i < 8; i++){
				for(var j = 0; j < 4; j++){
					a.push((h[i] >>> (8*j)) & 0xff | 0x100);
				}
			}
			return String.fromCharCode(...a);
		},
	zero:	[ 0,0,0,0, 0,0,0,0, ],
};
