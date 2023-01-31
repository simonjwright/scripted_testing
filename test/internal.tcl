puts "reading the script:\n"

echo "call 'first', with argument 'last'"
first last

echo "\ncall 'lists', with arguments 'a', 'b', 'c'"
lists a b c

echo "\ncall 'except', with argument 'an_exception_to_raise'"
except an_exception_to_raise

echo "\ncall 'first', with argument 'again'"
first again

puts "\nnow running the script. Will end with a CE, to be ignored.\n"
go
