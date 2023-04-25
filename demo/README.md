# Demo #

The Software under Test in this demonstration of Scripted Testing uses the services of a package `Digital_IO`, which manages Input and Output signals. When an Input signal changes, the corresponding Output signal is set to the inverse of the input.

## <a name="digital_io.ads">Digital IO</a> ##

`Digital_IO` notifies its user of signal changes using a _callback_ interface (an implementation of the [Observer pattern](https://en.wikipedia.org/wiki/Observer_pattern)). The system of which the SUT is a part provides a generic callback server, in the package `Sample_Callbacks`.

```
with Sample_Callbacks;

package Digital_IO is

   subtype Input_Signal
     is Integer
     range 0 .. 15;
   --  Numbers the input (switch) signal pins.

   subtype Output_Signal
     is Integer
     range 0 .. 15;
   --  Numbers the output (lamp) signal pins.

   type Input_Signal_State is record

      S : Input_Signal;
      --  The input signal that has changed.

      State : Boolean;
      --  The state that the signal has changed to.

   end record;

   package Input_Signal_State_Callbacks
   is new Sample_Callbacks (Input_Signal_State);
   --  Provides application domains with callback facilities for input
   --  signal state changes.

   function Get
     (S : Input_Signal)
     return Boolean;
   --  Returns the current state of the input signal for pin S.

   procedure Set
     (O : Output_Signal;
      To_State : Boolean);
   --  Sets the given output signal to the given state.

end Digital_IO;
```

### Scripting support ###

The Tcl functions that we _need_ for scripting support are

* `callback-digital_io.input_signal_state`, which takes a Tcl list argument containing the desired data
* `save_number_of_calls digital_io.set`
* `check_number_of_new_calls digital_io.set`, which takes an int argument saying how many calls there should have been to `digital_io.set` since the last save (or since the start of the script, if there hasn't been a save yet)
* `check-boolean-for-digital_io.output_signal`, which takes 5 arguments:
  * the subprogram concerned (in this case, `digital_io.set`)
  * the 'key' parameter (in this case, `o`, the output signal)
  * the key value for which the check is required
  * the 'value' parameter (in this case, `to_state`)
  * the required value

These let us write, for example,

```
echo "setting {1, true}"
save_number_of_calls digital_io.set
callback-digital_io.input_signal_state {1 true}
wait 0.001
check_number_of_new_calls digital_io.set 1
check-boolean-for-digital_io.output_signal \
    digital_io.set \
    o 1 \
    to_state false
```

(the `wait 0.001` is because, in general, the script and the SUT are running in different tasks, and we need to make sure that the SUT has had time to respond to the input change.)

### Stubbing support ###

What we need from [Digital IO](#digital_io.ads) is implementations of `Get` and `Set` that

* record when the subprogram is called
* record input parameters
* return output parameters and function results
* allow exceptions to be raised when required.

This is done (see `../generator/Makefile.inc`) by

* generating an XML representation of the package structure, using [libadalang2xml](https://github.com/simonjwright/libadalang2xml)
* from this, generating code using an XSLT script.

Considering `Set`, the generated code in `digital_io.adb` consists of the body stub

```
   procedure Set
     (O        : Output_Signal;
      To_State : Boolean) is separate;
```

and, in the package's handled sequence of statements, its registration:

```
   Scripted_Testing.Stubs.Register_Subprogram ("Digital_IO.Set");

   Scripted_Testing.Stubs.Register_Input_Parameter ("Digital_IO.Set", "O");

   Scripted_Testing.Stubs.Register_Input_Parameter
     ("Digital_IO.Set", "To_State");
```

The reason for implementing `Set` in a proper body is that, for some subprograms, you might want to make adjustments; for example, a time-of-validity would need to be related to the time at which the script was executed.

Not necessary in this case:

```
separate (Digital_IO)
procedure Set
  (O        : Output_Signal;
   To_State : Boolean)
is
```

In case of access by multiple tasks, acquire a lock.

```
   Lock : Scripted_Testing.Stubs.Lock (Scripted_Testing.Stubs.Mutex'Access);
   pragma Unreferenced (Lock);
```

Note which call this is.

```
   Call : constant Positive :=
     Scripted_Testing.Stubs.Note_Entry ("Digital_IO.Set");
begin
```

Record the input parameters. They're recorded in memory-based streams; `Get_Input_Value_Stream` allocates a stream of sufficient size, and we use `'Output` to fill the stream with the data.

```
   Output_Signal'Output
     (Scripted_Testing.Stubs.Get_Input_Value_Stream
        ("Digital_IO.Set", "O", Call, O'Size),
      O);
   Boolean'Output
     (Scripted_Testing.Stubs.Get_Input_Value_Stream
        ("Digital_IO.Set", "To_State", Call, To_State'Size),
      To_State);
```

We may need to raise an exception.

```
   Scripted_Testing.Stubs.Check_For_Exception ("Digital_IO.Set", Call);

end Set;
```

### Tcl interface ###

Some of the required Tcl functions are provided for free: once a subprogram has been registered, `check_number_of_calls`, `save_number_of_calls` and `check_number_of_new_calls` are available.

We need to implement the other Tcl functions. Support is provided in `Scripted_Testing.Stubs.Scripting` and `Scripted_Testing.Callbacks`, and our use is in `digital_io/digital_io-scripting.adb`.

For `check-boolean-for-digital_io.output_signal`, we instantiate `Scripting.Check_Keyed_Value`:

```
   package Check_Boolean_For_Output_Signal
     is new Scripted_Testing.Stubs.Scripting.Check_Keyed_Value
       (Checked_Type      => Boolean,
        Checked_Type_Name => "boolean",
        Checked_Value     => Boolean'Value,
        Checked_Image     => Boolean'Image,
        Key_Type          => Output_Signal,
        Key_Type_Name     => "digital_io.output_signal",
        Key_Value         => Output_Signal'Value,
        Key_Image         => Output_Signal'Image);
```

`callback-digital_io.input_signal_state` is somewhat more complicated, because of the need for a _value_ function to convert the list `{1, true}` into an `Input_Signal_State`:

```
   function Input_Signal_State_Value (S : String)
                                     return Digital_IO.Input_Signal_State
   is
      Tokens : GNAT.String_Split.Slice_Set;
   begin
      GNAT.String_Split.Create (S          => Tokens,
                                From       => S,
                                Separators => " " & ASCII.HT,
                                Mode       => GNAT.String_Split.Multiple);
      if Natural (GNAT.String_Split.Slice_Count (Tokens)) /= 2 then
         raise Constraint_Error
           with "digital_io.input_signal_state requires 2 components";
      end if;
      return Result : Input_Signal_State do
         Result.S :=
           Input_Signal'Value (GNAT.String_Split.Slice
                                 (Tokens,
                                  GNAT.String_Split.Slice_Number (1)));
         Result.State :=
           Boolean'Value (GNAT.String_Split.Slice
                            (Tokens,
                             GNAT.String_Split.Slice_Number (2)));
      end return;
   end Input_Signal_State_Value;
```

after which we can instantiate `Callbacks`:

```
   package Input_Signal_State_Callbacks
   is new Scripted_Testing.Callbacks
     (T                  => Digital_IO.Input_Signal_State,
      Callback_Type_Name => "digital_io.input_signal_state",
      Value              => Input_Signal_State_Value,
      Call_Callbacks     =>
        Digital_IO.Input_Signal_State_Callbacks.Call_Callbacks);
```

----

This may seem like quite a lot of work, but it can be reused without change for

* changes to the SUT
* other users of `Digital_IO`.
