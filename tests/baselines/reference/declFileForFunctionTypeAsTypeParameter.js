//// [declFileForFunctionTypeAsTypeParameter.ts]
class X<T> {
}
class C extends X<() => number> {
}
interface I extends X<() => number> {
}



//// [declFileForFunctionTypeAsTypeParameter.js]
var __extends = (this && this.__extends) || (function () {
    var extendStatics = function (d, b) {
        extendStatics = Object.setPrototypeOf ||
            ({ __proto__: [] } instanceof Array && function (d, b) { d.__proto__ = b; }) ||
            function (d, b) { for (var p in b) if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p]; };
        return extendStatics(d, b);
    };
    return function (d, b) {
        extendStatics(d, b);
        function __() { this.constructor = d; }
        d.prototype = b === null ? Object.create(b) : (__.prototype = b.prototype, new __());
    };
})();
var X = /** @class */ (function () {
    function X() {
    }
    return X;
}());
var C = /** @class */ (function (_super) {
    __extends(C, _super);
    function C() {
        return _super !== null && _super.apply(this, arguments) || this;
    }
    return C;
}(X));


//// [declFileForFunctionTypeAsTypeParameter.d.ts]
declare class X<T> {
}
declare class C extends X<() => number> {
}
interface I extends X<() => number> {
}
