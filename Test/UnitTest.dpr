program UnitTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  SysUtils,
  {$IFDEF FPC}
  fpcunit, testregistry, consoletestrunner { you can add units after this },
  {$ENDIF}
  {$IFDEF DCC}
  TextTestRunner,
  {$ENDIF}
  Tests;

{$IFDEF FPC}
var
  App: TTestRunner;
{$ENDIF}

begin
  {$IFDEF DCC}
  TextTestRunner.RunRegisteredTests(rxbHaltOnFailures);
  {$ENDIF}

  {$IFDEF FPC}
  // Provide no-arguments launch
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  App := TTestRunner.Create(nil);
  App.Initialize;
  App.Run;
  App.Free;
  {$ENDIF}

  Writeln('Asserts: ', TTplTest.TotalAssertCnt);
  Sleep(1000);
end.
