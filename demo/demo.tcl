# Copyright (C) 2023 Simon Wright <simon@pushface.org>
# SPDX-License-Identifier: CC0-1.0

# We have to give the application time to accept inputs; waiting
# (delaying) means that the application task can run.
set PAUSE 0.001

wait 1.0
echo "setting {0, false}"
callback-digital_io.input_signal_state {0 false}
wait $PAUSE
check_number_of_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 0 \
    to_state true

wait 1.0
echo "setting {1, true}"
save_number_of_calls digital_io.set
callback-digital_io.input_signal_state {1 true}
wait $PAUSE
check_number_of_new_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 1 \
    to_state false

wait 1.0
echo "setting {15, false}"
save_number_of_calls digital_io.set
callback-digital_io.input_signal_state {15 false}
wait $PAUSE
check_number_of_new_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 15 \
    to_state true

go


