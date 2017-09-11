'use strict';

ArrayBuffer.prototype.toStringEx = function() {

	var str = "";
	var dataView = new DataView(this);
	for (var i=0; i<dataView.byteLength; i++) {
		str += dataView.getUint8(i).toString(16).padLeft("0", 2) + " ";
	}
	
	return str.trim();
}

ArrayBuffer.prototype.equals = function(other) {
	core.Contract.assert(other instanceof ArrayBuffer, "Expected ArrayBuffer.");

	if (this.byteLength != other.byteLength) {
		return false;
	}

    var dv1 = new DataView(this);
    var dv2 = new DataView(other);
    for (var i = 0 ; i != this.byteLength ; i++) {
        if (dv1.getUint8(i) != dv2.getUint8(i)) {
        	return false;
        }
    }

    return true;
}

ArrayBuffer.prototype.startsWith = function(other) {
	core.Contract.assert(other instanceof ArrayBuffer, "Expected ArrayBuffer.");

	if (this.byteLength < other.byteLength) {
		return false;
	}

	var dv1 = new DataView(this);
    var dv2 = new DataView(other);
    for (var i = 0 ; i != other.byteLength ; i++) {
        if (dv1.getUint8(i) != dv2.getUint8(i)) {
        	return false;
        }
    }

    return true;
}
