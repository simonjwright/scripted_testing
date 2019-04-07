# timing.tcl

# can we wait?
echo "waiting 4"
wait 4

# set a mark
echo "marking a"
mark a

# wait for 3 seconds
echo "waiting 3"
wait 3

echo "wait for 3 seconds complete"

# wait for 4 seconds from the mark, of which only 1 second is left
echo "waiting for 4 seconds from mark a - only 1 second left"
wait_from_mark a 4

echo "wait for what should have been the one remaining second complete"

go
