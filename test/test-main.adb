procedure Test.Main is
begin
   Scripted_Testing.Register (The_Command => new First,
                              To_Be_Named => "first");
   Scripted_Testing.Start;
end Test.Main;
