# test.tcl

puts "Tests on basic commands"

puts "* 'echo' with no arguments should fail"
if [catch {echo} msg] {
    if ![ regexp {'echo' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'echo' should have failed"
    exit 1
}

puts "* 'echo' with two arguments should fail"
if [catch {echo a b} msg] {
    if ![ regexp {'echo' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'echo a b' should have failed"
    exit 1
}

puts "* 'go' with one argument should fail"
if [catch {go a} msg] {
    if ![ regexp {'go' requires zero arguments} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'go a' should have failed"
    exit 1
}

puts "* 'mark' with no arguments should fail"
if [catch {mark} msg] {
    if ![ regexp {'mark' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'mark' should have failed"
    exit 1
}

puts "* 'mark' with two arguments should fail"
if [catch {mark a b} msg] {
    if ![ regexp {'mark' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'mark a b' should have failed"
    exit 1
}

puts "* 'wait' with no arguments should fail"
if [catch {wait} msg] {
    if ![ regexp {'wait' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait' should have failed"
    exit 1
}

puts "* 'wait' with two arguments should fail"
if [catch {wait a b} msg] {
    if ![ regexp {'wait' requires 1 argument} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait a b' should have failed"
    exit 1
}

puts "* 'wait' with invalid argument should fail"
if [catch {wait a} msg] {
    if ![ regexp {bad input for 'Value: "a"} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait a' should have failed"
    exit 1
}

puts "* 'wait_from_mark' with one argument should fail"
if [catch {wait_from_mark a} msg] {
    if ![ regexp {'wait_from_mark' requires 2 arguments} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait_from_mark a' should have failed"
    exit 1
}

puts "* 'wait_from_mark' with three arguments should fail"
if [catch {wait_from_mark a b c} msg] {
    if ![ regexp {'wait_from_mark' requires 2 arguments} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait_from_mark a b c' should have failed"
    exit 1
}

puts "* 'wait_from_mark' with invalid argument should fail"
if [catch {wait_from_mark a b} msg] {
    if ![ regexp {bad input for 'Value: "b"} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait_from_mark a b' should have failed"
    exit 1
}

mark mrk
wait_from_mark undefined 0.25

if [catch {go} msg] {
    if ![ regexp {no mark 'undefined'} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait_from_mark undefined 0.25' should have failed"
    exit 1
}

mark will_fail
wait 0.5
wait_from_mark will_fail 0.25

if [catch {go} msg] {
    if ![ regexp {mark 'will_fail' passed} $msg] then {
        puts "unexpected result '$msg'"
        exit 1
    }
} else {
    puts "'wait_from_mark will_fail 0.25' should have failed"
    exit 1
}
