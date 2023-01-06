-- basic program that uses base libraries and that should be used as a test for compilation

-- defining custom function, hello world of the functional world, factorial
fact :: Int -> Int
fact 1 = 1
fact 2 = 2
fact n = n * fact (n-1)

-- our program should read a number from terminal, calculate factorial and print it

-- action is a keyword (as opposed to function) that has side effects
-- main is a reserved entry point into a program
action main = 
    print "Hello, enter number: " -- primop action, via ffi?
    n::Int = readline -- readline is a primop action via ffi, but here we have an implicit conversion to Int from string -- how to handle it???
    if n < 1 
        print "Cannot calculate factorial of numbers below 1!"; main -- chaining actions together
    else 
        print (n + "! = " + fact n); main -- here print only accepts Strings, so implicitly converting n and fact n to Strings 
        


-- another idea - via Events, which might be more elegant

-- first defining an action that calculates factorial from an int and prints the result
-- This action is Reactive, with n::Int observing events of specific types and it is 
-- automatically re-run every time this event is fired!!! Cool?
-- Event structure, syntax and types need to be really thought through
-- reaction <name> on - is a special type of action that subscribes to events
reaction gotInput on e::Event UserInput
    n::Int = (Int) e.userInput -- needs thinking, don't like casts here, needs to be a function?
    if n < 1 
        print "Cannot calculate factorial of numbers below 1!" 
    else 
        print (n + "! = " + fact n); print "Enter another number: "

action main = 
    print "Hello, enter number: "
    startTerminalEventsLoop -- built in action that handles events in the "Terminal" environment / Node