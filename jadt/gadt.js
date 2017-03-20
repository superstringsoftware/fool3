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

    Any: new BuiltinType("Any", function() {return true;}),
    Number: new BuiltinType("Number", function(v) {return (typeof v === "number")}),
    String: new BuiltinType("String", function(v) {return (typeof v === "string")}),

    isType: function(tp) {
        return ((tp instanceof BuiltinType) || (tp instanceof ProductConstructor));
    }

}

// built in types
function BuiltinType(name, func) {
    this._name = name;
    this.isValue = func;
}


// sum types - basically a hashmap of product constructors
function SumType (name, constructors) {
    this._name = name;
    this._constructors = constructors;
    Types[name] = this;
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
    }
}

// fields = [{name : String, type : Types}]
// fields = [Types]
// if the fields is empty or first element doesn't have a "name" - store and handle as unnamed record
// _fields contains an object that maps field names to Types
function ProductConstructor  (name, fields) {
    this._name = name;
    this._fields = {};
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
                if (! Types.isType(t)) throw "Tried to create a ProductConstructor field of undefined type";
                this._fields[i] = t;
            }
        }
    }
}

ProductConstructor.prototype = {
    // assign a value to a specific field with type checking
    assign: function(fieldName, value) {
        this._values[fieldName] = value;
    },
    // create a new value from this constructor
    create: function(valueList) {
        //console.log("Creating object");
        // creating empty value of this type
        var val = new Value(this);
        for (var i = 0; i<valueList.length; i++) {
            if (this._fields.hasOwnProperty(i)) {
                t = this._fields[i];
                if (t.isValue(valueList[i]))
                    val._value[i] = valueList[i];
                else throw "Tried to assign a value of illegal type"
            }
            else throw "Tried to initialize a value with non-existent field"
        }
        return val;
    },
    // checks if v is of this type
    isValue: function(v) {
        return ((v instanceof Value) && (v.cons === this));
    },
    // pretty print type signature
    show: function() {
        console.log(this._name, this._fields);
    }
};



// ****************************************************************************

function tests() {
    var info = function(t) {
        console.dir(t, {depth: null, colors: true});
    };
    console.log("anonymous record type:");
    var t1 = new ProductConstructor("Pair", [Types.Any, Types.Number]);
    info(t1, {depth: null, colors: true});
    console.log("record with named fields:");
    var t2 = new ProductConstructor("Point",
        [{name: "x", type: Types.Number},
         {name: "y", type: Types.Number}
    ]);
    info(t2);
    console.log("Value of Pair:");
    var v1 = t1.create(["hello", 1]);
    info(v1);
    console.log(t1.isValue(v1));
    console.log(t2.isValue(v1));

}

tests();


/*
var just = new ProductConstructor("Just", [{name: "0", type: Types.Any}]);
var nothing = new ProductConstructor("Nothing");

var just1 = just.create({0: 1});

nothing.show();
just.show();
//just1.show();

var maybea = new SumType("Maybe", [nothing, just]);
console.log(maybea);

var cons = new ProductConstructor("Cons", [{name: "0", type: "any"}, {name: "0", type: "List"}] );

console.log(Types);

t = new Value(Types.Any);
console.log(t);
k = new Value(10);

var List = {
    Nil: "Nil"
}

var Cons = function (val, list) {
    ret = {};
    ret[0] = val;
    ret[1] = list;
    return ret;
}

var l1 = Cons (1, Cons(2, Cons (3, List.Nil)));

console.log (l1);
*/
