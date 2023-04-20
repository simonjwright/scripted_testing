# Copyright (C) 2023 Simon Wright <simon@pushface.org>
# SPDX-License-Identifier: CC0-1.0

echo "wait 1"
wait 1.0
echo "setting {0, false}"
callback-digital_io.input_signal_state {0 false}
wait 0.000_001
check_number_of_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 0 \
    to_state true

echo "wait 1"
wait 1.0
echo "setting {1, true}"
save_number_of_calls digital_io.set
callback-digital_io.input_signal_state {1 true}
wait 0.000_001
check_number_of_new_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 1 \
    to_state false

echo "wait 1"
wait 1.0
echo "setting {15, false}"
save_number_of_calls digital_io.set
callback-digital_io.input_signal_state {15 false}
wait 0.000_001
check_number_of_new_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 15 \
    to_state true

go


