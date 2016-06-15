// module Test.Bench

exports.foldlfilt = function(pred){
    return function(f){
        return function(z){
            return function(arr){
                var acc = z;
                for(var i = 0; i < arr.length; i++){
                    acc = f(acc, arr[i]);
                }
                return acc;
            }
        }
    }
}
