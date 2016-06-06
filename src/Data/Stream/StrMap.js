// module Data.Stream.StrMap

var Data_Stream = require ("../Data.Stream");

exports.playeff = function(next){
    return function(s0){
        return function(f){
            return function(){
                var s = s0;
                while(true){
                    var x = next(s);
                    if(x instanceof Data_Stream.Yield){
                        f(x.value0)();
                        s = x.value1;
                    } else if(x instanceof Data_Stream.Skip){
                        s = x.value0;
                    } else if(x instanceof Data_Stream.Done){
                        break;
                    } else {
                        throw "playeff bug";
                    }
                }
                return;
            }
        }
    }
}
