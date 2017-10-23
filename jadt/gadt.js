/*
General idea:
Have a global object that holds information on all SumTypes in the system -
need to look them up by name
Every SumType consists of name -> constructorFunction mappings and varuous
type checking functions
Every constructorFunction is basically a Product type and has to support
recursion (see list example below), initialized by it's name and a list
of type values (eventually to support records will have to change to (name, type))
pairs.
*/

// all types in the system
var Types = {
    // initializing with built-in js types
    // dummy for recursive SumTypes definition
    __SELF__: new BuiltinType("__SELF__", function() {return false;}),
    Any: new BuiltinType("Any", function() {return true;}),
    Number: new BuiltinType("Number", function(v) {return (typeof v === "number")}),
    String: new BuiltinType("String", function(v) {return (typeof v === "string")}),

    isType: function(tp) {
        return ((tp instanceof BuiltinType) ||
                (tp instanceof ProductConstructor) ||
                (tp instanceof SumType));
    },

    // find a constructor by name
    findConstructor: function(name) {
        for (tp in this) {
            if (this[tp] instanceof SumType) {
                //console.log(tp, typeof this[tp]);
                if (this[tp].C[name]) return this[tp].C[name];
            }
        }
        throw "Constructor " + name + " doesn't exist";
    }

}

// built in types
function BuiltinType(name, func) {
    this._name = name;
    this.isValue = func;
    this.show = name;
}



// sum types - basically a hashmap of product constructors
function SumType (name, constructors) {
    this._name = name;
    //this._constructors = constructors;
    this._consMap = {};
    // mapping constructors for named access
    for (var i = 0; i<constructors.length; i++) {
        this._consMap[constructors[i]._name] = constructors[i];
        // processing any recursive type definitions if found
        for (prop in constructors[i]._fields) {
            var t = constructors[i]._fields[prop];
            if (t === Types.__SELF__) {
                //console.log("Found recursion!");
                constructors[i]._fields[prop] = this;
            }
        }
    }
    // processing recursion
    Types[name] = this;
}

SumType.prototype = {
    // check if a Value is of this type
    isValue: function(v) {
        for (var cons in this._consMap) {
            //console.log(cons);
            if (this._consMap[cons].isValue(v)) return true;
        }
        return false;
    },
    get C() {
        return this._consMap;
    },
    // pretty print type signature
    get show() {
        // _SHOW_RECURSION_ is used to display recursive types properly
        if (this._SHOW_RECURSION_) return ("("+this._name+")");
        this._SHOW_RECURSION_ = true;
        str = "data " + this._name + " = ";
        for (var prop in this._consMap)
            str += this._consMap[prop].show + " | ";
        str = str.substring(0, str.length-3);
        delete this._SHOW_RECURSION_;
        return str;
    }
}

// boxed values of ProductConstructors - think how to unbox built in types
function Value (consType) {
    if (! Types.isType(consType)) throw "Tried to create a value of undefined type";
    this._consType = consType, // one of Types.xxx
    this._value = {};
}

Value.prototype = {
    get value() {
        return this._value;
    },
    get cons() {
        return this._consType;
    },
    // pretty print value
    get show() {
        var str = this._consType.name;
        //console.log("entering SHOW call");
        //console.log(this);
        // it's an anonymous record
        if (this._value.hasOwnProperty(0) || Object.keys(this._value).length == 0) {
            for (var prop in this._value) {
                //console.log("Processing " + prop);
                var val = this._value[prop];
                //console.log(val);
                var txt = "";
                if (val instanceof Value) {
                    //console.log('Processing boxed');
                    txt = "(" + val.show +")";
                }
                else {
                    //console.log('Processing UNboxed');
                    txt = val;  // (if it's a boxed value need to call a function to unbox)
                }
                str += " " + txt;
            }
        }
        else {
            /*
            str += " { ";
            //console.log(this._fields);
            for (var prop in this._fields) {
                //console.log(this._fields[prop]);
                str += prop + "::" + this._fields[prop].show + "; ";
            }
            str+= "}";*/
        }
        return str;
    },
}

// fields = [{name : String, type : Types}]
// fields = [Types]
// if the fields is empty or first element doesn't have a "name" - store and handle as unnamed record
// _fields contains an object that maps field names to Types
function ProductConstructor  (name, fields) {
    this._name = name;
    this._fields = {};
    //this._belongsTo = null;
    if (Array.isArray(fields) && (fields.length > 0)) {
        f = fields[0];
        // it's a record - [{name : String, type : Types}]
        if (f.hasOwnProperty("name")) {
            for (var i = 0; i<fields.length; i++) {
                var t = fields[i].type;
                if (! Types.isType(t)) throw "Tried to create a ProductConstructor field of undefined type";
                this._fields[fields[i].name] = t;
            }
        }
        // it's an anonymous product type - [Types]
        else {
            for (var i = 0; i<fields.length; i++) {
                var t = fields[i];
                //console.log(t);
                if (! Types.isType(t)) throw "Tried to create a ProductConstructor field of undefined type";
                this._fields[i] = t;
            }
        }
    }
}

ProductConstructor.prototype = {
    // assign a value to a specific field with type checking
    assign: function(fieldName, value) {
        //this._values[fieldName] = value;
    },
    // create a new value from this constructor
    create: function(valueList) {
        //console.log("Creating object");
        // creating empty value of this type
        var val = new Value(this);
        if (Array.isArray(valueList)) {
            for (var i = 0; i<valueList.length; i++) {
                if (this._fields.hasOwnProperty(i)) {
                    t = this._fields[i];
                    if (t.isValue(valueList[i]))
                        val._value[i] = valueList[i];
                    else throw "Tried to assign a value of illegal type"
                }
                else throw "Tried to initialize a value with non-existent field"
            }
        }
        return val;
    },
    // checks if v is of this type
    isValue: function(v) {
        return ((v instanceof Value) && (v.cons === this));
    },
    // pretty print type signature
    get show() {
        str = this._name;
        // it's an anonymous record
        if (this._fields.hasOwnProperty(0) || Object.keys(this._fields).length == 0) {
            for (var prop in this._fields)
                str += " " + this._fields[prop].show;
        }
        else {
            str += " { ";
            //console.log(this._fields);
            for (var prop in this._fields) {
                //console.log(this._fields[prop]);
                str += prop + "::" + this._fields[prop].show + "; ";
            }
            str+= "}";
        }
        return str;
    },
    get name() {
        return this._name;
    }
};


var _l = {
    // definition of a new datatype
    /* _l.data ("Tree", {
                 Leaf: [Types.Any],
                 Branches: [Types.__SELF__, Types.__SELF__]
             });
    ) */
    data: function(name, cons) {
        var constructors = [];
        for (prop in cons) {
            constructors.push(new ProductConstructor(prop,cons[prop]));
        }
        //console.log(constructors);
        var ret = new SumType(name, constructors);
        //console.log(ret);
        return ret;
    },

    T: Types, // alias for Types

    //instanciate new value based on a constructor name
    // _l.n ("Just", [5]) --> Just 5
    n: function (consName, values) {
        var cons = Types.findConstructor(consName);
        //console.log(cons);
        return cons.create(values);
    },

    // pattern matching on constructors
    match: function (expr, pattern, func) {
        console.log(expr);
        console.log(pattern);
        console.log(func);

    }
}


// ****************************************************************************

function info (t) {
    //console.log(t);
    console.dir(t, {depth: null, colors: true});
};

function tests() {

    var t0 = new ProductConstructor("Strange", [Types.Any]);
    var t01 = new ProductConstructor("Weird", [Types.Any]);
    var vt1 = t0.create([1]);
    var vt2 = t01.create([vt1]);
    info(vt2);
    info(vt2.show);

    console.log("\nanonymous record type:");
    var t1 = new ProductConstructor("Pair", [Types.Any, Types.Number]);
    info(t1.show);
    console.log("\nrecord with named fields:");
    var t2 = new ProductConstructor("Point",
        [{name: "x", type: Types.Number},
         {name: "y", type: Types.Number}
    ]);
    info(t2.show);
    console.log("\nValue of Pair:");
    var v1 = t1.create(["hello", 1]);
    info(v1.show);
    //info(v1);
    var v2 = t1.create([v1, 5]);
    info(v2.show);
    //info(v2);
    console.log(t1.isValue(v1));
    console.log(t2.isValue(v1));
    console.log("\nMaybe a = Nothing | Just a");
    var mt = new SumType("Maybe",
                         [
                             new ProductConstructor("Nothing"),
                             new ProductConstructor("Just", [Types.Any])
                         ]);
    info(mt.show);
    console.log("\nJust a examples:");
    var noth = mt.C.Nothing.create();
    info(noth.show);
    console.log (t1.isValue(noth));
    console.log (mt.isValue(noth));
    info(mt.C.Just.create([1]).show);
    info(mt.C.Just.create(["hello"]).show);
    console.log("\nList a = Nil | Cell a (List a)");
    var lt = new SumType("List",
                         [
                             new ProductConstructor("Nil"),
                             new ProductConstructor("Cell", [Types.Any, Types.__SELF__])
                         ]);
    info(lt.show);
    var vl = Types.List.C.Cell.create([1,
             Types.List.C.Cell.create([2,
             Types.List.C.Nil.create()]) ]   );
    info(vl.show);
    //console.log("\nAll types dump:");
    //info(Types);
    var x = Types.findConstructor("Nil");
    //console.log(x);
    //Types.findConstructor("Nilasf");
    var tree = _l.data ("Tree", {
                 Leaf: [Types.Any],
                 Branches: [Types.__SELF__, Types.__SELF__]
             }
    );
    var booltype = _l.data ("Bool", {True: [], False: []});
    info(tree.show);
    info(booltype.show);
    info(_l.n("True").show);

    var tval = _l.n ("Nil");
    var tval1 = _l.n ("Just", [5]);
    var tval2 = _l.n ("Cell", [1, _l.n("Cell", [2, _l.n("Nil")])]);
    info(tval.show);
    info(tval1.show);
    info(tval2.show);

    info(Types);

}

function calculator() {
    var calcADT = _l.data ("Expr", {
        Value: [Types.Number],
        Add: [Types.__SELF__, Types.__SELF__],
        Mul: [Types.__SELF__, Types.__SELF__]
    });
    info(calcADT.show);

    var valE = _l.n ("Value",[10]);
    info(valE.show);

    var addE = _l.n ("Add", [valE, _l.n("Value", [20])]);
    info (addE.show);

    // evaluation function
    var eval = function (e) {
        // how many params to match
        _l.match (e, { Value: 'x'}, function(x) {console.log(x);});
        _l.match (e, { Add:
                        [{ Value: 'x'},
                         { Value: 'y'}]
                     }, function(x,y) {return false;});
    }

    eval(valE);
    //eval(addE);
}

tests();
calculator();
